package sazagotchi

import java.time.OffsetDateTime
import scala.io.StdIn.{ readLine, readChar }
import LifeStage._

/***********************
 * The Tamagotchi game *
 ***********************/
object Play extends App {
  // Setup
  val name = readLine("What will your Tamagotchi be called? ")
  val now = OffsetDateTime.now()
  val eggShelfLife = now.plusNanos(egg.totalDur.toNanos)
  val tama = new TamaShot(name, LifeStage.stdLifeCycle, now, Some(egg), now,
                          eggShelfLife, eggShelfLife, eggShelfLife, 0, 0, now)
  println(s"> Great, now you're the happy carer of ${tama.name}!")

  /** Iteratively grow the Tamagotchi */
  def iterate(tamaStr: Stream[TamaShot]): Unit = tamaStr match
  {
    case Stream.Empty => println("** Thanks for playing! **")
    case (h #:: tl) =>
      if (tl.isEmpty) {
        println(s"\n[HEALTH REPORT FOR ${h.name}]\n" + h.healthReport)
        println("** Thanks for playing! **")
      } else {
        val t = tl.head
        println(t.healthReport)
        println(s"What would you like to give ${t.name}? " +
          s"(F)ood, (S)leep, (C)leanup, (M)edication")
        readChar() match {
          case 'F' => if (!t.ill) iterate(t.feed) else {
            println("> Too ill, won't eat."); iterate(tl.tail) }
          case 'S' =>  if (!t.ill) iterate(t.sleep) else {
            println("> Too ill, won't sleep."); iterate(tl.tail) }
          case 'C' =>  if (!t.ill) iterate(t.clean) else {
            println("> Too ill, cleaning not enough."); iterate(tl.tail) }
          case 'M' =>  if (t.ill) iterate(t.medicate) else {
            println("> Healthy, won't take meds."); iterate(tl.tail) }
          case 'Q' => println("** Thanks for playing! **")
          case _ => println("> Sorry, don't know this one..."); iterate(tl.tail)
        }
      }
  }

  iterate(tama.toStream)

}
