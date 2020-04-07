package game
import scala.collection.mutable.Buffer

object labGame extends App{

  
  def newMaze(floors: Int, height: Int, width: Int) = {
    val a = Buffer[Floor]()
    for(i <- 1 to floors){
      a += new Floor(height, width)
    }
    val maze = new Maze(a)
    maze.generate
    maze
  }
  
  
  val maze = newMaze(3, 25, 25)
  maze.solve
  val txtMaze = maze.floors.map(_.toTxt)
  
  //print(txtMaze.map(_.mkString).mkString("\n"))
  
  for(i <- txtMaze.indices){
    println((txtMaze(i).map(_.mkString).mkString("\n")) + "\n")
}
}