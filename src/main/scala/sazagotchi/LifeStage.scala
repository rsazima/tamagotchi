package sazagotchi

import java.time.Duration

/**
 * Describes the Tamagotchi life stages
 *
 * name: Life stage name
 * stageHours: How many Tamagotchi hours this life stage lasts
 * food: How many Tamagotchi hours until it needs to eat
 * sleep: How many Tamagotchi hours until it needs to sleep
 * poop: How many Tamagotchi hours until it needs to poop
 * hourDur: Actual duration of 1 hour of Tamagotchi's life
 */

final class LifeStage(val name: String, val stageHours: Int, val hourDur: Duration,
                      val food: Int, val sleep: Int, val poop: Int)
{
  lazy val totalDur: Duration = hourDur.multipliedBy(stageHours)
  override def toString = name
}

object LifeStage {
  val stdHourDur: Duration = Duration.ofMillis(2000)
  val egg   = new LifeStage("Egg",   1 * 1 * 12, stdHourDur, 24, 24, 24)
  val child = new LifeStage("Child", 1 * 2 * 24, stdHourDur,  6, 12, 12)
  val teen  = new LifeStage("Teen",  1 * 2 * 24, stdHourDur,  6, 24, 24)
  val adult = new LifeStage("Adult", 1 * 4 * 24, stdHourDur,  8, 24, 24)
  val elder = new LifeStage("Elder", 1 * 4 * 24, stdHourDur, 10, 24, 24)
  val stdLifeCycle = Vector(egg, child, teen, adult, elder)
}
