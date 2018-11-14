import org.scalatest.FlatSpec

class CafeXSpec extends FlatSpec {

  "CafeX getColByIndex" should "return a list of length 4 for column index 1" in {

    val col = CafeXMain.getColByIndex(1, CafeXMain.stock)
    assert(col.length == 4)
  }

  "CafeX getColByIndex" should "return a list of length 4 for column index 1 of which the second element should be Coffee" in {

    val col = CafeXMain.getColByIndex(1, CafeXMain.stock)
    assert(col(1) == "Coffee")
  }

  "CafeX getelementByProduct" should "return '0.50' for product Cola" in {

    val price = CafeXMain.getElementByProduct("Cola", CafeXMain.stock, 4)
    assert(price == "0.50")
  }

  "CafeX getelementByProduct" should "return '2.00' for product Cheese Sandwich" in {

    val price = CafeXMain.getElementByProduct("Cheese Sandwich", CafeXMain.stock, 4)
    assert(price == "2.00")
  }

  "CafeX standardBill" should "return '3.5' for list [“Cola”, “Coffee”, “Cheese Sandwich”]" in {

    val order = Array("Cola", "Coffee", "Cheese Sandwich")
    val total = CafeXMain.standardBill(order)
    assert(total == 3.5)
  }

  "CafeX serviceCharge" should "return '0.35' for list [“Cola”, “Coffee”, “Cheese Sandwich”]" in {

    val order = Array("Cola", "Coffee", "Cheese Sandwich")
    val total = CafeXMain.serviceCharge(order, CafeXMain.standardBill(order))
    assert(total == "0.35")
  }

  "CafeX serviceCharge" should "return '1.20' for list [“Cola”, “Coffee”, “Steak Sandwich”]" in {

    val order = Array("Cola", "Coffee", "Steak Sandwich")
    val total = CafeXMain.serviceCharge(order, CafeXMain.standardBill(order))
    assert(total == "1.20")
  }

  "CafeX serviceCharge" should "return '20.00' for list [“Cola”, “Coffee”, “Steak Sandwich”]" in {

    val order = Array("Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich","Steak Sandwich")
    val total = CafeXMain.serviceCharge(order, CafeXMain.standardBill(order))
    assert(total == "20.00")
  }
}
