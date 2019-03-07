package cs320

package object hw01 extends Homework01 {
  // Problem 1

  // I assum that when we have minus values, we scale minus values
  def dollar2won(dollar: Int): Int = {
    dollar*1100
  }

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = {
    a*b*c
  }
  def isEven(num: Int): Boolean = {
    val check:Int = num%2
    if(check == 0){
      true
    }
    else{
      false
    }
  }
  def isOdd(num: Int): Boolean = {
    !isEven(num)
  }
  def gcd(a: Int, b: Int): Int = {
    //search for 0
    if(a == 0 || b == 0){
      if(a == b){
        0
      }
      else if(a > b){
        a
      }
      else{
        b
      }
    }
    //classic Euclidean algorithm by division
    else if(a >= b){
      if(a%b == 0){
        b
      }
      else {
        gcd(b, a % b)
      }
    }
    else{
      if(b%a == 0){
        a
      }
      else {
        gcd(a, b % a)
      }
    }

  }
  def lcm(a: Int, b: Int): Int = {
    // from multiplication a*b we "delete" all common factors, so once we divide by gcd(a,b)
    // search for 0
    if (a == 0 && b == 0){
      0
    }
    else{
      (a*b)/ gcd(a, b)
    }

  }

  // Problem 2
  def numOfHomework(course: COURSE): Int = course match{
    case CS320(quiz: Int, homework: Int) => homework
    case CS311(homework: Int) => homework
    case CS330(projects: Int, homework: Int) => homework
  }

  def hasProjects(course: COURSE): Boolean = course match{
    case CS320(quiz: Int, homework: Int) => false
    case CS311(homework: Int) => false
    case CS330(projects: Int, homework: Int) => true
  }

  // functions for storing (consumed, and unconsumed string

  // Problem 3
  def namePetsMatch(pet: String): String = pet match{
      case "dog" => "happy"
      case "cat" => "smart"
      case "pig" => "pinky"
      case _ => pet
  }

  // head - only first
  // tail - without first
  // last - only last
  // init - without last
  def namePetsRec(pets_consumed: List[String], pets_unconsumed: List[String]): List[String] = {
      if(pets_unconsumed == List()){
        pets_consumed
      }
      else{
        //take last and append
        val pet_name = namePetsMatch(pets_unconsumed.last)
        namePetsRec(pet_name :: pets_consumed, pets_unconsumed.init)
      }
  }

  def namePets(pets: List[String]): List[String] = {
      namePetsRec(List(), pets)
  }

  def giveNameMatch(pet: String, oldName: String, newName: String): String = {
    if(pet == oldName) {
      newName
    }
    else {
      pet
    }
  }

  def giveNameRec(pets_consumed: List[String], pets_unconsumed: List[String], oldName: String, newName: String): List[String] = {
    if(pets_unconsumed == List()){
      pets_consumed
    }
    else{
      //take last and append
      val pet_name = giveNameMatch(pets_unconsumed.last, oldName, newName)
      giveNameRec(pet_name :: pets_consumed, pets_unconsumed.init, oldName, newName)
    }
  }

  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    (toConsumeList: List[String]) => giveNameRec(List(), toConsumeList, oldName, newName)
  }

  def tests: Unit = {
    //dollar2won
    test(dollar2won(1), 1100)

    //columeOfCuboid
    test(volumeOfCuboid(1, 2, 3), 6)
    test(volumeOfCuboid(10, 10, 10), 1000)

    //isEven
    test(isEven(10), true)
    test(isEven(11), false)

    //isOdd
    test(isOdd(10), false)
    test(isOdd(11), true)

    //gcd
    test(gcd(123, 245), 1)
    test(gcd(100, 10), 10)
    test(gcd(0, 20), 20) //this said google
    test(gcd(20, 0), 20) //this said google
    test(gcd(0, 0), 0) //this said google


    //lcm
    test(lcm(123, 245), 30135)
    test(lcm(10, 123), 1230)


    //numOfHomework
    test(numOfHomework(CS320(quiz = 4, homework = 3)), 3)
    test(numOfHomework(CS330(projects = 4, homework = 3)), 3)

    //hasProjects
    test(hasProjects(CS320(quiz = 3, homework = 9)), false)
    test(hasProjects(CS330(projects = 3, homework = 9)), true)
    test(hasProjects(CS311(homework = 12)), false)


    //namePets
    test(namePets(List("dog", "cat", "pig")), List("happy", "smart", "pinky"))
    test(namePets(List("", "cat", "pig")), List("", "smart", "pinky"))

    //giveName
    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))
    test(giveName("bearr", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "bear"))

    /* Write your own tests */
  }
}