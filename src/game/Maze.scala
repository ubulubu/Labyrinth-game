package game
import scala.util.Random
import scala.collection.mutable.Buffer

class Maze (val floors: Buffer[Floor]){
  
  def generate = {
    this.floors.map(_.prim)
    this
  }
  
  def printMaze = {
    this.floors.map(_.toTxt)
  }
  
  def solve = {
    this.floors.map(_.solver)
    this
  }

}
