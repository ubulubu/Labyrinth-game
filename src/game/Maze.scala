package game
import scala.util.Random
import scala.collection.mutable.Buffer

class Maze (val floors: Buffer[Floor]){
  // applies prim to all of the floors
  def generate = {
    this.floors(0).prim(1, 1)
    this.floors(1).prim(this.floors(0).last.x, this.floors(0).last.y)
    this.floors(2).prim(this.floors(1).last.x, this.floors(1).last.y)
    this
  }
 
}
