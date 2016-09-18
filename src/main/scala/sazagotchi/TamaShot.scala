package sazagotchi

import java.time.{ Duration, OffsetDateTime }
import scala.util.Random

/**
 * A "shot" of the Tamagotchi life at a given point in time
 */
class TamaShot (
  val name: String,
  lifeCycle: Vector[LifeStage],
  createdAt: OffsetDateTime,
  val stage: Option[LifeStage],
  stageChange: OffsetDateTime,
  lastMeal: OffsetDateTime,
  lastSleep: OffsetDateTime,
  lastPoop: OffsetDateTime,
  bedTraining: Int,
  toiletTraining: Int,
  now: OffsetDateTime = OffsetDateTime.now()
  )
{
  import sazagotchi.TamaShot.Level._

  val age: Duration = Duration.between(createdAt, now)

  val isAlive: Boolean = {
    val deathAge = lifeCycle.map(s => s.hourDur.toNanos * s.stageHours).sum
    (deathAge > age.toNanos) && (health < DEAD)
  }

  lazy val foodHealth = stage.map(s => levelFor(lastMeal, s.food, s.hourDur))
  lazy val sleepHealth = stage.map(s => levelFor(lastSleep, s.sleep, s.hourDur))
  lazy val poopHealth = stage.map(s => levelFor(lastPoop, s.poop, s.hourDur))

  lazy val health: Value = Vector(foodHealth, sleepHealth, poopHealth)
    .map(_.getOrElse(DEAD)).max

  val ill: Boolean =
    if (Set(SERIOUS, CRITICAL).contains(health)) true else false

  val healthReport: String = (
      s"\n[NAME: $name AGE: $age]\n"
        + s"[STAGE: ${stage.map(_.toString).getOrElse("Dead")}]\n"
        + s"[HEALTH: $health ${if (ill) "** ILL **" else ""}]\n"
        + s" - Food:  ${foodHealth.getOrElse(DEAD).toString.toLowerCase}\n"
        + s" - Sleep: ${sleepHealth.getOrElse(DEAD).toString.toLowerCase}\n"
        + s" - Poop:  ${poopHealth.getOrElse(DEAD).toString.toLowerCase}"
        //+ debugInfo
      )

  def feed: Stream[TamaShot] = {
    val (stage, stChange) = evolve
    new TamaShot(name, lifeCycle, createdAt, stage, stChange, OffsetDateTime.now(),
      lastSleep, lastPoop, bedTraining, toiletTraining).toStream
  }

  def sleep: Stream[TamaShot] = {
    val (stage, stChange) = evolve
    new TamaShot(name, lifeCycle, createdAt, stage, stChange, lastMeal,
      OffsetDateTime.now(), lastPoop, bedTraining + 1, toiletTraining).toStream
  }

  def clean: Stream[TamaShot] = {
    val (stage, stChange) = evolve
    new TamaShot(name, lifeCycle, createdAt, stage, stChange, lastMeal,
      lastSleep, OffsetDateTime.now(), bedTraining, toiletTraining + 1).toStream
  }

  def medicate: Stream[TamaShot] = {
    val (stage, stChange) = evolve
    val now = OffsetDateTime.now()
    new TamaShot(name, lifeCycle, createdAt, stage, stChange,
      now, now, now, bedTraining, toiletTraining).toStream
  }

  /** Tamagotchi life is a stream of "shots", one for each point in time */
  def toStream: Stream[TamaShot] = if (!isAlive) Stream.Empty else {
    val still = howYouDoin
    still #:: still.toStream
  }

  /** Private members */
  private def howYouDoin: TamaShot = {
    val (stage, stChange) = evolve
    val sleepTime = if (sleepDue && (bedTraining > 8)
      && (Random.nextInt(11) < bedTraining)) OffsetDateTime.now() else lastSleep
    val poopTime = if (poopDue && (toiletTraining > 8)
      && (Random.nextInt(11) < toiletTraining)) OffsetDateTime.now() else lastPoop
    new TamaShot(name, lifeCycle, createdAt, stage, stChange,
      lastMeal, sleepTime, poopTime, bedTraining, toiletTraining)
  }

  private def sleepDue: Boolean = stage.exists { s =>
    sleepHealth.exists { level =>
      val sinceLast = Duration.between(lastSleep, now).toNanos
      val delta = s.hourDur.multipliedBy(s.sleep * (level.id + 1)).toNanos - sinceLast
      val sleepPeriod = s.hourDur.multipliedBy(s.sleep).toNanos
      if (delta.toDouble / sleepPeriod.toDouble > 0.8) true else false
    }
  }

  private def poopDue: Boolean = stage.exists { s =>
    poopHealth.exists { level =>
      val sinceLast = Duration.between(lastPoop, now).toNanos
      val delta = s.hourDur.multipliedBy(s.poop * (level.id + 1)).toNanos - sinceLast
      val poopPeriod = s.hourDur.multipliedBy(s.poop).toNanos
      if (delta.toDouble / poopPeriod.toDouble > 0.8) true else false
    }
  }

  private def evolve: (Option[LifeStage], OffsetDateTime) =
    if (stage.isEmpty) (stage, stageChange) else {
      val sinceLast = Duration.between(stageChange, now).toNanos
      val stageDur = stage.get.totalDur.toNanos
      if (sinceLast < stageDur) (stage, stageChange) else {
        val newLifeCycle = lifeCycle.dropWhile(_ != stage.get).drop(1)
        if (newLifeCycle.isEmpty) (stage, now) else
          (newLifeCycle.headOption, OffsetDateTime.now)
      }
    }

  private def levelFor(lastTime: OffsetDateTime, tamaHours: Int,
                       hourDur: Duration): Value =
  {
    val sinceLast = Duration.between(lastTime, now).toNanos
    values.dropWhile { v =>
      hourDur.multipliedBy(tamaHours * (v.id + 1)).toNanos < sinceLast
    }.headOption.getOrElse(DEAD)
  }

  private def debugInfo: String = (
      s"\n - createdAt: $createdAt\n"
      + s" - lastMeal:  $lastMeal\n"
      + s" - lastSleep: $lastMeal\n"
      + s" - lastPoop:  $lastMeal\n"
      + s" - bedTrain:  $bedTraining\n"
      + s" - toiletTr:  $toiletTraining\n"
      + s" - now:       $now\n"
    )

}

object TamaShot {
  /***
   * Health levels (https://en.wikipedia.org/wiki/Medical_state).
   * Also applied for: Food, Sleep, Cleanliness
   *  - EXCELLENT, GOOD, FAIR are healthy
   *  - SERIOUS, CRITICAL represents illness
   */
  object Level extends Enumeration {
    type Level = Value
    val EXCELLENT, GOOD, FAIR, SERIOUS, CRITICAL, DEAD = Value
  }
}
