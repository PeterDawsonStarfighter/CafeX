/**
  * Peter Dawson solution to CafeX Task for HMRC
  * 14/11/2018
  *
  * This is the main class providing the Billing implementation. For the purposes of
  * this exercise, the Billing functions will be called from the Test classes to generate the output
  */
object CafeXMain {

  //  Model the stock as a 2D matrix approximating what the data would look like if it was in a database. Hardcode will be 'simplest' solution for this exercise
  //  The hypothetical DB columns are: product, serving, type, price
  //  The prices are stored here as type String to keep it simple instead of storing them as double as could be done but if stored as double here then the code to select the column from the data would need to be type Any or something in order to pick our columns that could be either String or double. The more simple solution is to cast the string value to double when needed that to elaborate the logic
  val stock = Array( Array("Cola", "Cold", "Drink", "0.50"), Array("Coffee", "Hot", "Drink", "1.00"), Array("Cheese Sandwich", "Cold", "Food", "2.00"), Array("Steak Sandwich", "Hot", "Food", "4.50"))

  def main(args: Array[String]): Unit = {

    println("#### CafeXMain started ####")
    println("\n")

    val order = Array("Cola", "Coffee", "Cheese Sandwich")
    val total = CafeXMain.standardBill(order)
    println("The Step 1 answer is: " + total + "\n")

    val sc = CafeXMain.serviceCharge(order, CafeXMain.standardBill(order))
    println("The Step 2 service charge on that is: " + sc + "\n")

  }

  /**
    * Get column n from the multidimensional array
    * This method simplified by making the price column a String value instead of a double => all columns of type String
    *
    * @param n - the column index - starts at 1!
    * @param a - the multi-dimensional array
    * @return
    */
  def getColByIndex(n: Int, a: Array[Array[String]]): Array[String] = a.map{_(n - 1)}

  /**
    * Get the specified element for a specific product
    * Slightly clunky to traverse the columsn multiple times... presumably there is an idomatic 1-liner
    * @param product - the product
    * @param a - the array of products
    * @param elementIndex - 1 indexed!
    * @return
    */
  def getElementByProduct(product : String, a: Array[Array[String]], elementIndex : Int): String = {

    //  Get the product column
    val column = getColByIndex(1, CafeXMain.stock)

    //  What row is the product we got passed
    val row = column.indexOf(product)

    //  Return the matching price
    getColByIndex(elementIndex, CafeXMain.stock)(row)
  }

  /**
    * The answer to Step 1
    * Stick with Arrays for the data and convert to List to iterate
    *
    * There is nothing to round the number of digits returned here to 1 place to generate the answer in the PDF although the test case works and test doesn't say to round to 1 DP, so ambiguous?
    *
    * @param list - the things ordered
    * @return
    */
  def standardBill(list : Array[String]) : Double = {

    var runningTotal : Double = 0.00

    //  Iterate over the Bill and generate the total
    val x = list.toList
    for(product <- x){

      runningTotal += getElementByProduct(product, CafeXMain.stock, 4).toDouble
    }
    runningTotal
  }

  /**
    * Work out the service charge
    * @param list - the things ordered
    * @param bill - the bill amount
    * @return
    */
  def serviceCharge(list : Array[String], bill : Double) : String = {

    object AllDone extends Exception { }
    var serviceCharge = 0.0

    //  Iterate over the Bill and calculate the service charge
    val x = list.toList

    try {
      for (product <- x) {

        if (getElementByProduct(product, CafeXMain.stock, 3).equals("Food")) {
          //  Set the SC to 0
          serviceCharge = 10.0

          //  If the food is also hot then SC = 20% and exit
          if (getElementByProduct(product, CafeXMain.stock, 2).equals("Hot")) {

            serviceCharge = 20.00
            // break out
            throw AllDone
          }
        }
      }
    }catch{
      case AllDone =>
    }

    //  Take the percentage of the total
    var sc = bill * (serviceCharge/100)
    //  SC can't be more than 20
    if(sc > 20)
      sc = 20
    //  round and return
    f"$sc%.2f"
  }
}
