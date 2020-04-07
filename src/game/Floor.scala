package game
import scala.util.Random
import scala.collection.mutable.Buffer

class Floor (val height: Int, val width: Int) {
  // Generate maze with only walls
  var board = {
    val a = Array.ofDim[cell](height, width)
      for(i <- 0 until height){
        for(j <- 0 until width){
          a(i)(j) = new cell(i, j)
        }
      }
    a
  }
  
  val a = Random.nextInt(height - 2) + 1
  val b = Random.nextInt(width - 2) + 1
  
  // Helper method for checking valid coordinates  
  def isLegal(coords: (Int, Int)) = {
    (coords._2 > 0 && coords._2 < this.width - 1) && (coords._1 > 0 && coords._1 < this.height - 1)
    
  }
  // Helper method for Prim
  def inBetween(num1: Int, num2: Int):Int = {
    val p = num1 - num2
    p match {
      case 0 => num1
      case 2 => num2 + 1
      case -2 => num1 + 1
      case _ => 213812831 // should throw error
    }
  }
  // Helper method for Prim, calculates the frontiercells.
  var frontierList = Buffer[cell]()
  def frontierCell (x: Int, y: Int) = {
    val coords = Buffer[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    this.frontierList ++= coords
    this.frontierList = this.frontierList.filter(!_.isPassable).distinct
    this.frontierList
  }
  // Helper method for Prim, calculates the "neighbors" aka frontiercells with passable set to true
  def neighbor (x: Int, y: Int) = {
    var neighborList = Buffer[cell]()
    val coords = Buffer[(Int, Int)]((x-2, y), (x+2, y), (x, y-2), (x, y+2)).filter(isLegal(_)).map(x => this.board(x._1)(x._2))
    neighborList ++= coords
    neighborList = neighborList.filter(_.isPassable).distinct
    neighborList(Random.nextInt(neighborList.size))
  }
  // Helper method for Prim, sets a random cell in between neighbot and frontiercell to passable
  def connectRandomNeighbor(x1: Int, y1: Int) = {
    
    val b = neighbor(x1, y1)
    
    val x = inBetween(x1, b.x)
    val y = inBetween(y1, b.y)
    
    this.board(x)(y).setPassable
    }
 //algorithm attempt
  
  val first = {
      this.board(a)(b).setPassable
      this.board(a)(b).setStart
      this.board(a)(b)
    }
  //placeholder
  var goal = this.board(0)(0)
  
  def prim = {
    var i = 0
    var square = frontierCell(first.x, first.y)(Random.nextInt(frontierList.size))
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
      //i += 1
      }
    goal = this.frontierList.head
    connectRandomNeighbor(goal.x, goal.y)
    this.board(goal.x)(goal.y).setPassable
    board(goal.x)(goal.y).setGoal
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
        else if (this.board(i)(j).isStart) txt(i)(j) = "S"
        else if (this.board(i)(j).isSolution) txt(i)(j) = "."
        //else if (this.board(i)(j).isVisited) txt(i)(j) = "x"
        else if(this.board(i)(j).isPassable) txt(i)(j) = " "
        else txt(i)(j) = "#"
      }
    }
  txt
  }
  
  var path = Buffer[cell]()
  val coordList = Buffer[(Int, Int)]((0, 1), (1, 0), (0, -1), (-1, 0))
  
  def explorer (coords: (Int, Int)): Boolean = {
    if(!this.isLegal(coords) || !this.board(coords._1)(coords._2).isPassable || this.board(coords._1)(coords._2).isVisited){
      return false
      }
    
    this.board(coords._1)(coords._2).setVisited
    path += board(coords._1)(coords._2)
    
    if (this.board(coords._1)(coords._2).isGoal) {
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
  def solver = {
    explorer(this.first.x, this.first.y)
    for(i <- path){
      this.board(i.x)(i.y).setSolution
    }
    val op = path.map(a => (a.x, a.y))
    println(op.size)
  }
  def pooper = {
    for(i <- path){
      this.board(i.x)(i.y).setSolution
    }
  }
}