package sazagotchi

import java.time.Duration

/**
 * Test fixtures
 */
trait Fixtures {
  val stdHourDur: Duration = Duration.ofMillis(100)
  val egg   = new LifeStage("Egg",    3, stdHourDur, 99, 99, 99)
  val child = new LifeStage("Child", 24, stdHourDur,  6, 12, 12)
  val teen  = new LifeStage("Teen",  24, stdHourDur,  6, 16, 16)
  val adult = new LifeStage("Adult", 24, stdHourDur,  8, 16, 16)
  val elder = new LifeStage("Elder", 24, stdHourDur,  10, 20, 20)
}
