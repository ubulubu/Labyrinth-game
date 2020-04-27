package game
import scala.util.Random
import scala.collection.mutable.Buffer

class Floor (val height: Int, val width: Int) {
  // Generate maze with only walls
  var board = {
    val a = Array.ofDim[Cell](height, width)
      for(i <- 0 until height){
        for(j <- 0 until width){
          a(i)(j) = new Cell(i, j)
        }
      }
    a
  }
  
  // Helper method for checking valid coordinates  
  private def isLegal(coords: (Int, Int)) = {
    (coords._2 > 0 && coords._2 < this.width - 1) && (coords._1 > 0 && coords._1 < this.height - 1)
    
  }
  // Helper method for Prim
  private def inBetween(num1: Int, num2: Int):Int = {
    val p = num1 - num2
    p match {
      case 0 => num1
      case 2 => num2 + 1
      case -2 => num1 + 1
      case _ => 1234  // Should throw error
    }
  }
  // Helper method for Prim, calculates the frontiercells.
  private var frontierList = Buffer[Cell]()
  def frontierCell (x: Int, y: Int) = {
    val coords = Buffer[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    this.frontierList ++= coords
    this.frontierList = this.frontierList.filter(!_.isPassable).distinct
    this.frontierList
  }
  // Helper method for Prim, calculates the "neighbors" aka frontiercells with passable set to true
  private def neighbor (x: Int, y: Int) = {
    var neighborList = Buffer[Cell]()
    val coords = Buffer[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    neighborList ++= coords
    neighborList = neighborList.filter(_.isPassable).distinct
    neighborList(Random.nextInt(neighborList.size))
  }
  // Helper method for Prim, sets a random cell in between neighbot and frontiercell to passable
  private def connectRandomNeighbor(x1: Int, y1: Int) = {
    
    val b = neighbor(x1, y1)
    
    val x = inBetween(x1, b.x)
    val y = inBetween(y1, b.y)
    
    this.board(x)(y).setPassable
    }
  //placeholder coordinates
  var first = this.board(1)(1)
  
    //placeholder coordinates
  var last = this.board(2)(2)
  
  def prim(a:Int, b:Int) = {
    first = this.board(a)(b)
    this.board(a)(b).setPassable
    this.board(a)(b).setDown(true)
    this.board(a)(b)
    var square = frontierCell(a, b)(Random.nextInt(frontierList.size))
    connectRandomNeighbor(square.x, square.y)
    this.board(square.x)(square.y).setPassable
    this.frontierList = frontierCell(square.x, square.y)
    this.frontierList -= square
    
    while(this.frontierList.size > 1){
      square = frontierList(Random.nextInt(frontierList.size))
      this.frontierList = frontierCell(square.x, square.y)
      
      connectRandomNeighbor(square.x, square.y)
      this.board(square.x)(square.y).setPassable
      this.frontierList -= square
     
      }
      last = this.frontierList.head
      connectRandomNeighbor(last.x, last.y)
      this.board(last.x)(last.y).setPassable
      board(last.x)(last.y).setDown(true)
      this.board
  }
  
  
  var txt = {
    val a = Array.ofDim[String](height, width)
    for(i <- 0 until height){
        for(j <- 0 until width){
          a(i)(j) = "#"
        }
      }
    a
  }
  // 2D array containing the maze as characters
  def toTxt = {
    for (i <- 0 until height){
      for(j <- 0 until width){
        if (this.board(i)(j).isGoal) txt(i)(j) = "G"
        else if (this.board(i)(j).containsP) txt(i)(j) = "P"
        else if (this.board(i)(j).isStart) txt(i)(j) = "S"
        else if (this.board(i)(j).isUp) txt(i)(j) = "/"
        else if (this.board(i)(j).isDown) txt(i)(j) = raw"\"
        else if (this.board(i)(j).isSolution) txt(i)(j) = "."
        else if(this.board(i)(j).isPassable) txt(i)(j) = " "
        else txt(i)(j) = "#"
      }
    }
  txt
  }
  
  private var path = Buffer[Cell]()
  private val coordList = Buffer[(Int, Int)]((0, 1), (1, 0), (0, -1), (-1, 0))
  
  // Helper for solving the floor, by finding a route to Target cell
  private def explorer (coords: (Int, Int)): Boolean = {
    if(!this.isLegal(coords) || !this.board(coords._1)(coords._2).isPassable || this.board(coords._1)(coords._2).isVisited){
      return false
      }
    
    this.board(coords._1)(coords._2).setVisited
    path += board(coords._1)(coords._2)
    
    if (this.board(coords._1)(coords._2).isTarget) {
      return true
    }
    
    for(dir <- coordList){
      val coord = (coords._1 + dir._1, coords._2 + dir._2)
      if(explorer(coord)) {
        return true
      }
    
    }
    path = path.init
    return false
    
  }
  // solves the current floor and returns the last cell
  def solver (x: Int, y: Int)= {
    explorer(x, y)
    for(i <- path){
      this.board(i.x)(i.y).setSolution
    }
    path.last
  }
}