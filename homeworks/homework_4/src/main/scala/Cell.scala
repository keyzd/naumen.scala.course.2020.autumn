import table._

package cell {
	trait Cell

	class EmptyCell extends Cell {
		override def toString(): String = "empty"
	}

	class NumberCell(value: Int) extends Cell {
		override def toString(): String = value.toString()
	}

	class StringCell(value: String) extends Cell {
		override def toString(): String = value.toString()
	}

	class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
			val hash: Int = ix+iy*table.width

			override def toString(): String = {
				if(ix >= table.width || iy >= table.height)
					"outOfRange"
				else {
					if(!table.hasHash(hash)) {
						table.addHash(hash)
						val nextCell = table.getCell(ix, iy).get
						nextCell match {
							case ref: ReferenceCell => ref.toString()
							case notRef: Cell => {
								table.dropHashes()
								notRef.toString()
							}
						}
					} else {
						"cyclic"
					}
				}
			}
	}
}
