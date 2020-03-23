package game

class cell (val x:Int, val y:Int){
  
  var isWall = true
  var isStaircase = false
  var isGoal = false
  var isGround = false
  
  def setGround = this.isGround = true

  def setStairs = this.isStaircase = true
  
  def setGoal = this.isGoal = true
}