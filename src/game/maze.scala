package game
import scala.util.Random

class maze (val height: Int, val width: Int, val floors: Int){
  
  var board = {
    val a = Array.ofDim[cell](height, width)
      for(i <- 0 until height){
        for(j <- 0 until width){
          a(i)(j) = new cell(i, j)
        }
      }
    a
  }
  
  val a = Random.nextInt(height)
  val b = Random.nextInt(width)
    
  def isLegal(coords: (Int, Int)) = {
    (coords._2 > 0 && coords._2 < this.width - 1) && (coords._1 > 0 && coords._1 < this.height - 1)
    
  }
  
  def inBetween(num1: Int, num2: Int):Int = {
    if (num1 > num2) num1 - 1 
    else if (num2 > num1) num2 - 1
    else if(num1 == num2) num1
    else num1
  }
  
  var frontierList = List[cell]()
  def frontierCell (x: Int, y: Int) = {
    val coords = List[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    frontierList = coords ++ frontierList
    frontierList.filter(!_.isPassable)
  }
  
  def neighbor (x: Int, y: Int) = {
    var neighborList = List[cell]()
    val coords = List[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    neighborList = coords ++ neighborList
    neighborList.filter(_.isPassable)
    neighborList(Random.nextInt(neighborList.size))
  }
  
  def connectRandomNeighbor(x1: Int, y1: Int) = {
    val b = neighbor(x1, y1)
    
    val x = inBetween(x1, b.x)
    val y = inBetween(y1, b.y)
    
    board(x)(y).setPassable
    board(x1)(y1).setPassable
    
    frontierList = frontierCell(x1, y1)
    
  }
 //algorythm attempt
  
  def Prim = {
    val first = {
      board(a)(b).setPassable
      board(a)(b)
    }
    var square = frontierCell(a, b)(Random.nextInt(frontierList.size))
    connectRandomNeighbor(square.x, square.y)
    frontierList = frontierList.take(frontierList.indexOf(square)) ++ frontierList.drop(frontierList.indexOf(square) + 1)
    
    while(frontierList.nonEmpty){
      square = frontierList(Random.nextInt(frontierList.size))
      connectRandomNeighbor(square.x, square.y)
      frontierList = frontierList.take(frontierList.indexOf(square)) ++ frontierList.drop(frontierList.indexOf(square) + 1)
      }
    board
  }

}
