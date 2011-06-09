package insane
package storage

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.H2Adapter


trait Storage {
  self: AnalysisComponent =>

  import global._


  def initializeStorage() {
    Class.forName("org.h2.Driver");

    SessionFactory.concreteFactory = Some(()=>
       Session.create(
         java.sql.DriverManager.getConnection("jdbc:h2:~/insane.db", "insane", ""),
         new H2Adapter))
  }

}
