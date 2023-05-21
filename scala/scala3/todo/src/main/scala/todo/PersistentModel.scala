package todo

import cats.implicits.*
import java.nio.file.{Path, Paths, Files}
import java.nio.charset.StandardCharsets
import io.circe.{Decoder, Encoder}
import io.circe.parser.*
import io.circe.syntax.*
import scala.collection.mutable
import todo.data.*
import scala.annotation.transparentTrait

/**
 * The PersistentModel is a model that saves all data to files, meaning that
 * tasks persist between restarts.
 *
 * You should modify this file.
 */
object PersistentModel extends Model:
  import Codecs.given

  /** Path where the tasks are saved */
  val tasksPath = Paths.get("tasks.json")
  /** Path where the next id is saved */
  val idPath = Paths.get("id.json")
  

  /**
   * Load Tasks from a file. Return an empty task list if the file does not exist,
   * and throws an exception if decoding the file fails.
   */
  def loadTasks(): Tasks =
    if Files.exists(tasksPath) then
      load[Tasks](tasksPath)
    else 
      Tasks.empty

  /**
   * Load an Id from a file. This Id is guaranteed to have never been used before.
   * Returns Id(0) if the file does not exist, and throws
   * an exception if decoding the file fails.
   */
  def loadId(): Id =
    if Files.exists(idPath) then
      load[Id](idPath)
    else
      val id = Id(0)
      id
      

  /**
   * Load JSON-encoded data from a file.
   *
   * Given a file name, load JSON data from that file, and decode it into the
   * type A. Throws an exception on failure.
   *
   * It is not necessary to use this method. You should be able to use loadTasks
   * and loadId instead, which have a simpler interface.
   */
  def load[A](path: Path)(using decoder: Decoder[A]): A = {
    val str = Files.readString(path, StandardCharsets.UTF_8)
    val result = decode[A](str)
    // In a production system we would want to pay more attention to error
    // handling than we do here, but this is sufficient for the case study.
    decode[A](str) match {
      case Right(result) => {
        result
      }
      case Left(error) => {
        throw error
      }
    }
  }

  /**
   * Save tasks to a file. If the file already exists it is overwritten.
   */
  def saveTasks(tasks: Tasks): Unit =
    //println(s"before $tasks")
    val sortedTask = Tasks(tasks.toList.sortBy(_._1))
    //println(s"after $sortedTask")
    save(tasksPath, sortedTask)

  /**
   * Save Id to a file. The Id saved to a file must be an Id that was never used before.
   * If the file already exists it is overwritten.
   */
  def saveId(id: Id): Unit =
    save(idPath, id)

  /**
   * Save data to a file in JSON format.
   *
   * Given a file name and some data, saves that data to the file in JSON
   * format. If the file already exists it is overwritten.
   *
   * It is not necessary to use this method. You should be able to use saveTasks
   * and saveId instead, which have a simpler interface.
   */
  def save[A](path: Path, data: A)(using encoder: Encoder[A]): Unit =
    val json = data.asJson
    Files.writeString(path, json.spaces2, StandardCharsets.UTF_8)
    //System.out.println(Files.readString(path));
    ()

  /* Hint: there are two pieces of state we need to implement the model:
   * - the tasks
   * - the next Id
   * (The InMemoryModel uses the same.)
   */
  private val idStore: mutable.LinkedHashMap[Id, Task] = mutable.LinkedHashMap()

  def create(task: Task): Id =
    val id = loadId()
    val nid = id.next
    saveId(nid)
    val tasks = Tasks((nid,task) :: loadTasks().toList)
    saveTasks(tasks)
    nid

  def read(id: Id): Option[Task] =
    val tasks = loadTasks().toList
    val task = tasks.find(t => t._1 == id) match 
                                            case Some(id,task) => Some(task)
                                            case None => None
                                            
    task
    

  def update(id: Id)(f: Task => Task): Option[Task] =
    val existingTask = read(id: Id)
    
    val update_task = existingTask match 
                              case Some(task) => { 
                                  val old_tasks = loadTasks().toList
                                  val filtered_tasks = old_tasks.filter(t => t._1.toInt != id.toInt)
                                  val updatedTask = existingTask.map(f).get
                                  val tasks = Tasks((id,updatedTask) :: filtered_tasks)
                                  saveTasks(tasks)
                                  Some(updatedTask)
                              }
                              case None => None
    
    update_task

  def delete(id: Id): Boolean =
    val existingTask = read(id: Id)
    existingTask match 
                              case Some(task) => { 
                                  val old_tasks = loadTasks().toList
                                  val filtered_tasks = old_tasks.filter(t => t._1.toInt != id.toInt)
                                  saveTasks(Tasks(filtered_tasks))
                                  true
                              }
                              case None => false
    
    


  def tasks: Tasks =
    loadTasks()
    

  def tasks(tag: Tag): Tasks =
    val t = tasks.toList.filter(v => v._2.tags.contains(tag))
    Tasks(t.toList)
    

  def complete(id: Id): Option[Task] =
    val task = read(id) 
    task match {
      case Some(t) => {
        val completed_task = t.complete
        update(id)(_ => completed_task)
        Some(completed_task)
      }
      case None => None
    }

  def tags: Tags =
    val v = tasks.toList.flatMap[Tag](v => v._2.tags)
    val vls = v.toList.toSet.toList
    Tags(vls)
  
  
  /**
  * Delete the tasks and id files if they exist.
  */
  def clear(): Unit =
    if Files.exists(idPath) then
      Files.delete(idPath)
    if Files.exists(tasksPath) then      
      Files.delete(tasksPath)
    
