package sazagotchi

import java.time.{OffsetDateTime, Duration}
import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.time._
import sazagotchi.TamaShot.Level

class TamaSpec extends FlatSpec with Matchers with Eventually with Fixtures {

  // TamaShot creation helper
  val newTama: () => TamaShot = { () =>
    val now = OffsetDateTime.now()
    val eggShelfLife = now.plusNanos(egg.totalDur.toNanos)
    new TamaShot("Hungry", Vector(egg, child, teen, adult, elder),
      now, Some(egg), eggShelfLife, eggShelfLife, eggShelfLife, now, 0, 0)
  }

  "Tamagotchi" should "be capable of aging" in {
    val stdHourDur: Duration = Duration.ofMillis(400)
    val egg   = new LifeStage("Egg",   1, stdHourDur, 99, 99, 99)
    val child = new LifeStage("Child", 1, stdHourDur, 99, 99, 99)
    val teen  = new LifeStage("Teen",  1, stdHourDur, 99, 99, 99)
    val adult = new LifeStage("Adult", 1, stdHourDur, 99, 99, 99)
    val elder = new LifeStage("Elder", 2, stdHourDur, 99, 99, 99)

    // Don't use newTama here because the above lifeStages won't be in scope
    val now = OffsetDateTime.now()
    val eggShelfLife = now.plusNanos(egg.totalDur.toNanos)
    val tama = new TamaShot("Ageable", Vector(egg, child, teen, adult, elder),
                   now, Some(egg), eggShelfLife, eggShelfLife, eggShelfLife, now, 0, 0)
    val iter = tama.toStream.iterator

    eventually(timeout(Span(100, Millis)), interval(Span(50, Millis))){
      iter.next().stage.get.name should be ("Egg") }
    eventually(timeout(Span(900, Millis)), interval(Span(50, Millis))){
      iter.next().stage.get.name should be ("Child") }
    eventually(timeout(Span(500, Millis)), interval(Span(50, Millis))){
      iter.next().stage.get.name should be ("Teen") }
    eventually(timeout(Span(500, Millis)), interval(Span(50, Millis))){
      iter.next().stage.get.name should be ("Adult") }
    eventually(timeout(Span(500, Millis)), interval(Span(50, Millis))){
      iter.next().stage.get.name should be ("Elder") }
    eventually(timeout(Span(500, Millis)), interval(Span(50, Millis))){
      if (iter.nonEmpty) iter.next(); iter.isEmpty should be (true) }
  }

  it should "lose health from hunger" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(2000, Millis)), interval(Span(50, Millis))){
      iter.next().foodHealth.get should be (Level.GOOD) }
  }

  it should "be capable of being fed" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(2000, Millis)), interval(Span(50, Millis))){
      iter.next().foodHealth.get should be (Level.GOOD) }
    val fedIter = iter.next().feed.iterator
    fedIter.next().foodHealth.get should be (Level.EXCELLENT)
  }

  it should "lose health from tiredness" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(4000, Millis)), interval(Span(50, Millis))){
      iter.next().sleepHealth.get should be (Level.GOOD) }
  }

  it should "be capable of being put to bed" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(4000, Millis)), interval(Span(50, Millis))){
      iter.next().sleepHealth.get should be (Level.GOOD) }
    val fedIter = iter.next().sleep.iterator
    fedIter.next().sleepHealth.get should be (Level.EXCELLENT)
  }

  it should "lose health from dirtyness" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(4000, Millis)), interval(Span(50, Millis))){
      iter.next().poopHealth.get should be (Level.GOOD) }
  }

  it should "be capable of being cleaned" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(4000, Millis)), interval(Span(50, Millis))){
      iter.next().poopHealth.get should be (Level.GOOD) }
    val fedIter = iter.next().clean.iterator
    fedIter.next().poopHealth.get should be (Level.EXCELLENT)
  }

  it should "get ill if health level is SERIOUS or CRITICAL" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(5000, Millis)), interval(Span(50, Millis))){
      iter.next().health should be (Level.SERIOUS) }
    iter.next().ill should be (true)
    eventually(timeout(Span(5000, Millis)), interval(Span(50, Millis))){
      iter.next().health should be (Level.CRITICAL) }
    iter.next().ill should be (true)
  }

  it should "be capable of being medicated" in {
    val iter = newTama().toStream.iterator
    eventually(timeout(Span(5000, Millis)), interval(Span(50, Millis))){
      iter.next().ill should be (true) }
    val fedIter = iter.next().medicate.iterator
    fedIter.next().ill should be (false)
  }

}
