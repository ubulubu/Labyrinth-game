package game

class cell (val x:Int, val y:Int){
  
  var isPassable = false
  var isStaircase = false
  var isGoal = false
  
  def setPassable = {
    this.isPassable = true
  }

  def setStairs = {
    this.isStaircase = true
  }
  
  def setGoal = {
    this.isGoal = true
  }
}