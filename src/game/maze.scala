package game
import scala.util.Random

class maze (val width: Int, val height: Int, val floors: Int){
  
  var board = {
    val a = Array.ofDim[cell](width, height)
      for(i <- 0 until height){
        for(j <- 0 until width){
          a(i)(j) = new cell(i, j)
        }
      }
    a
  }
  
  val a = Random.nextInt(width)
  val b = Random.nextInt(height)
  def randomCell = {
    board(a)(b).setGround
    board(a)(b)
  }
  
  def isLegal(current: cell) = {
    (current.x > 0 && current.x < this.width - 1) && (current.y > 0 && current.y < this.height - 1)
  }
  
  val first = randomCell
  
  var frontierList = List[cell]()
  def frontierCell (x: Int, y: Int) = {
    var frontierList = List[cell]()
    frontierList :+ board(x-2)(y) :+ board(x+2)(y) :+ board(x)(y-2) :+ board(x)(y+2)
    frontierList.filter(_.isWall).filter(isLegal(_))(Random.nextInt(frontierList.size))
  }
  
  def neighbor (x: Int, y: Int) = {
    var neighborList = List[cell]()
    neighborList :+ board(x-2)(y) :+ board(x+2)(y) :+ board(x)(y-2) :+ board(x)(y+2)
    neighborList.filter(_.isGround).filter(isLegal(_))(Random.nextInt(neighborList.size))
  }
  
  def connectRandomNeighbor(x1: Int, y1: Int) = {
    val a = frontierCell(x1, y1)
    val b = neighbor(x1, y1)
    
    val x = math.max(a.x, b.x) - math.abs(a.x - b.x)
    val y = math.max(a.y, b.y) - math.abs(a.y - b.y)
    
    board(x)(y).setGround
    
    val new_a = frontierCell(a.x, a.y)
    
    frontierList.drop(frontierList.indexOf(a))
  }
 //algorythm attempt
  val startX = first.x
  val startY = first.y
  
  def Prim = {
    var square = frontierCell(startX, startY)
    while(frontierList.nonEmpty){
      connectRandomNeighbor(square.x, square.y)
      square = frontierList(Random.nextInt(frontierList.size))
    }
  }

}
