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
          s"(F)ood, (S)leep, (C)leanup, (M)edication. Press H for Health Report.")
        readChar() match {
          case 'F' | 'f' =>
            if (!t.ill) { println("> Yummy! Blurp!"); iterate(t.feed) }
            else { println("> Too ill, won't eat.");  iterate(tl.tail) }
          case 'S' | 's' =>
            if (!t.ill) { println("> Zzzzzzzzzzz..."); iterate(t.sleep) }
            else { println("> Too ill, won't sleep."); iterate(tl.tail) }
          case 'C' | 'c' =>
            if (!t.ill) { println("> Nice and clean."); iterate(t.clean) }
            else { println("> Too ill, cleaning not enough."); iterate(tl.tail) }
          case 'M' | 'm' =>
            if (t.ill) { println("> Yuck! Tastes awful."); iterate(t.medicate) }
            else { println("> Healthy, won't take meds."); iterate(tl.tail) }
          case 'H' | 'h' => iterate(tl.tail)
          case 'Q' | 'q' => println("** Thanks for playing! **")
          case _ => println("> Sorry, don't know this one..."); iterate(tl.tail)
        }
      }
  }

  iterate(tama.toStream)

}
