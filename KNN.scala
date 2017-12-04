
import scala.util.Random
import scala.math

class Point{
  //in the simple example point is defined as 2-dim
  //we can use var x:Array[Float]=new Array[Float](n) to define n-dim point
  var x:Float=0
  var y:Float=0
  var l:String=""
  override def toString: String = {
    var s:String="("+x+","+y+"):"+l
    return s
  }
}

object KNN {
  //generate a cluster of points given label as l
  //x,y is the center of those points
  def generatePoints(x:Float,y:Float,n:Int,l:String):Array[Point]={
    var p:Array[Point]=new Array[Point](n)
    var i:Int=0
    var rand:Random=new Random(13)
    var radius:Float=10
    while(i<n){
        p(i)=new Point
        p(i).x=x+rand.nextFloat()*radius
        p(i).y=x+rand.nextFloat()*radius
        p(i).l=l
        i=i+1
    }
    return p
  }
  //cal distance between two points
  def calDist(p1:Point,p2:Point):Float={
      var distance:Double=math.sqrt(math.pow(p1.x-p2.x,2)+math.pow(p1.y-p2.y,2))
      return distance.toFloat
  }
  //return top k ponits except the point p itself
  //that rank in the order of distance to the given point
  //in a given Array of points
  def getTopK(p:Point,pA:Array[Point],k:Int):Array[Point]={
    var points=new Array[Point](k)
    var dist=new Array[Float](pA.length)
    //cal distance
    for(i<-0 to dist.length-1) {
      dist(i) = calDist(p, pA(i))
    }
    //rank top k+1 in case that p itself is in the array
    for(i<-0 to k){
      var minIndex=i
      for(j<-i+1 to dist.length-1)
        if(dist(j)<dist(minIndex)) {
            minIndex=j
        }
      var t=dist(minIndex)
      dist(minIndex)=dist(i)
      dist(i)=t
      var tp=pA(minIndex)
      pA(minIndex)=pA(i)
      pA(i)=tp
    }
    //do not add p to the top k points
    var j=0
    for(i<-0 to k;if(j<k)){
      if(pA(i)!=p) {
        points(j) = pA(i)
        j=j+1
      }
    }
    return points
  }
  //shuffle the order of generated points
  def shufflePoints(p:Array[Point]):Array[Point]={
      var p1=new Array[Point](p.length)

      return p1
  }
  //count the label presence of each label
  def countLabel(pA:Array[Point]):Map[String,Int]={
    var counter=Map[String,Int]()
    var v=0
    for(p<-pA){
      if(counter.contains(p.l)){
        v=counter(p.l)+1
        counter+=(p.l->v)
      }
      else{
        counter+=(p.l->1)
      }
    }
    return counter
  }
  //classfify point p according to its counter of top k points
  def classify(counter: Map[String,Int]):String={
    var maxK=""
    var maxV=0
    counter.keys.foreach(
      k=>if(counter(k)>maxV) {
        maxV=counter(k)
        maxK = k
      }
    )
    return maxK
  }
  //a simple test case with two clusters
  //p is labeled as r, p1 is labeled as b\
  //top k points for p2(0)
  //counter of labels frequency
  //show case of classify function
  def main(args: Array[String]): Unit = {
    val n:Short=100
    var p1=generatePoints(4,6,10,"r")
    var p2=generatePoints(6,8,7,"b")
    var points=Array.concat(p1,p2)
    var p=points(0)
    var top_p=getTopK(p,points,5)
    var counter=countLabel(top_p)
    println("top",5)
    for(i<-top_p)
      println(i)

    println()
    counter.keys.foreach(i=>println(i,counter(i)))
    println("real p is",p," is labeled as ",classify(counter))

  }
}
