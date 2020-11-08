import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import cell._

package table {
	class Table(val width: Int, val height: Int) {
		private var buffer = new ArrayBuffer[Cell](width*height)
		private var hashes = new ListBuffer[Int]

		for(i <- 0 to width*height-1)
			buffer.insert(i, new EmptyCell)

		def getCell(ix: Int, iy: Int): Option[Cell] = {
			if(ix >= width || iy >= height || ix < 0 || iy < 0)
				None
			else
				Option[Cell](buffer(ix+iy*width))
		}

		def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
			if(ix >= 0 && iy >= 0)
				buffer(ix+iy*width) = cell
		}

		def addHash(hash: Int): Unit = hashes += hash
		def dropHashes(): Unit = hashes = new ListBuffer[Int]
		def hasHash(hash: Int): Boolean = {
			if(hashes.indexOf(hash) == -1)
				false
			else
				true
		}
	}

}
