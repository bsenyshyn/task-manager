import Models._
import Models.{TaskRepository, UserRepository}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.Try

object Main {
  val db = Database.forConfig("ToDoList")

  val userRepository = new UserRepository(db)
  val taskRepository = new TaskRepository(db)

  def main(args: Array[String]): Unit = {
    //    init()
    //    fill()
    start()
  }

  def init(): Unit = {
    Await.result(db.run(UserTable.table.schema.create), Duration.Inf)
    Await.result(db.run(TaskTable.table.schema.create), Duration.Inf)
  }

  def fill(): Unit = {
    Await.result(userRepository.create(User("data", "data")), Duration.Inf)
    Await.result(userRepository.create(User("root", "root")), Duration.Inf)
  }

  def exec[T](action: DBIO[T]): T =
    Await.result(db.run(action), Duration.Inf)

  def check(choose: String): Int = {
    val problem = Try(choose.toInt)
    problem.getOrElse(-1)
  }

  def delay(): Unit = StdIn.readLine("Press -Enter- to proceed -> ")

  def isFinished(x: Boolean): String = if (x) "(Finished)" else "(Unfinished)"

  def addUser(login: String, password: String): Unit = userRepository.create(User(login, password))

  def addTask(login: String, taskText: String): Unit =
    Await.result(taskRepository.create(Task(login, taskText)), Duration.Inf)

  def deleteTask(id: Long) = Await.result(taskRepository.delete(id), Duration.Inf)

  def updateTask(id: Long, login: String, taskText: String, isDone: Boolean = false) =
    Await.result(taskRepository.update(Task(login, taskText, finished = isDone, taskId = id)), Duration.Inf)

  def createUser(usr: User): Unit = Await.result(userRepository.create(usr), Duration.Inf)

  def deleteUser(id: Long): Unit = Await.result(userRepository.delete(id), Duration.Inf)

  def start(): Unit = {
    val users = exec(UserTable.table.result)
    println("----Task Manager----\n")
    println("   Created users: ")
    println("--------------------")
    users.foreach(x => println("   "+ x.userId + "- " + x.login))
    println("--------------------")
    println("(1) Login")
    println("(2) Add new account")
    println("(3) Delete existing account")
    println("(4) Quit")

    val choose: String = scala.io.StdIn.readLine("Enter number: ")
    val num = check(choose)

    if(num == -1) {
      println("Wrong input! Try again.")
      start()
    }
    else {
      num match {
        case 1 => login()

        case 2 =>
          val usr_login = StdIn.readLine("Login: ")
          val usr_password = StdIn.readLine("Password: ")
          createUser(User(usr_login, usr_password))
          println("User was created.")
          delay()

        case 3 =>
          val usr_login = StdIn.readLine("Login: ")

          if(usr_login == "") start()
          if(!users.map(_.login).contains(usr_login)) {
            println("Invalid login! Try again or press -Enter- to return to main menu: ")
            login()
          }
          val usr_password = StdIn.readLine("Password: ")

          if(users.filter(_.login == usr_login).map(_.password).contains(usr_password)) {
            val userTasks = exec(TaskTable.table.filter(_.login === usr_login).result)
            for (x <- userTasks) deleteTask(x.taskId)
            deleteUser(users.filter(_.login == usr_login).head.userId)
            println("User was deleted!")
            delay()
          }
          else println("Invalid password! Try again")

        case 4 => sys.exit(0)

        case _ => println("Wrong number! Try again")
      }
      start()
    }

    def login(): Unit = {
      val usr_login = StdIn.readLine("Login: ")

      if(usr_login == "") start()
      if(!users.map(_.login).contains(usr_login)) {
        println("Invalid login! Try again or press -Enter- to return to main menu: ")
        login()
      }

      val usr_password = StdIn.readLine("Password: ")
      if(users.filter(_.login == usr_login).map(_.password).contains(usr_password))
        taskMenu(usr_login)
      else{
        println("Invalid password! Try again or press -Enter- to return to main menu: ")
        login()
      }
    }

    def taskMenu(l: String): Unit = {
      val tasks = exec(TaskTable.table.result)
      val userTasks = tasks.filter(t => t.login == l)

      println("--------------------")
      println("Tasks for user: " + l + "\n")
      userTasks.foreach(x => println(x.taskId + "- " + x.task + " " + isFinished(x.finished)))
      println("--------------------")
      println("(1) New task")
      println("(2) Delete task")
      println("(3) Edit task")
      println("(4) Finish the task")
      println("(5) Show finished tasks")
      println("(6) Show unfinished tasks")
      println("(7) To main menu")

      val choose: String = scala.io.StdIn.readLine("Enter number: ")
      val num = check(choose)

      if(num == -1) {
        println("Incorrect input! Try again")
        taskMenu(l)
      }
      else {
        num match {
          case 1 =>
            val taskValue = StdIn.readLine("Task: ")
            addTask(l, taskValue)
            println("Task was added.")
            delay()
            taskMenu(l)

          case 2 =>
            val taskId = Try(StdIn.readLine("Enter task ID: ").toInt).getOrElse(-1)
            if(taskId == -1 || userTasks.filter(_.taskId == taskId).isEmpty){
              println("Wrong id.")
              taskMenu(l)
            }
            else deleteTask(taskId)
            println("Task was deleted.")
            delay()
            taskMenu(l)

          case 3 =>
            val taskId = Try(StdIn.readLine("Enter task ID: ").toInt).getOrElse(-1)
            if(taskId == -1 || userTasks.filter(_.taskId == taskId).isEmpty){
              println("Wrong id.")
              taskMenu(l)
            }
            else{
              val newTask = StdIn.readLine("New task: ")
              updateTask(taskId, l, newTask)
              println("Task was updated.")
              delay()
              taskMenu(l)
            }

          case 4 =>
            val taskId = Try(StdIn.readLine("Enter task ID: ").toInt).getOrElse(-1)
            if(taskId == -1 || userTasks.filter(_.taskId == taskId).isEmpty){
              println("Wrong id.")
              taskMenu(l)
            }
            else updateTask(taskId, l, userTasks.filter(_.taskId == taskId).head.task, true)
            println("Succeed.")
            delay()
            taskMenu(l)

          case 5 =>
            println("--------------------\n")
            println("Done tasks: ")
            userTasks.filter(_.finished == true)
              .foreach(x => println(x.taskId + ". " + isFinished(x.finished) + " " + x.task))
            println("--------------------\n")
            delay()
            taskMenu(l)

          case 6 =>
            println("--------------------\n")
            println("Undone tasks: ")
            userTasks.filter(_.finished == false)
              .foreach(x => println(x.taskId + ". " + isFinished(x.finished) + " " + x.task))
            println("--------------------\n")
            delay()
            taskMenu(l)
          case 7 => start()
        }
      }
    }
  }
}
