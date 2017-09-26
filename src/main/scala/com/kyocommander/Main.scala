package com.kyocommander

import com.danielasfregola.twitter4s.TwitterRestClient
import com.danielasfregola.twitter4s.TwitterStreamingClient
import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App {

  args.toList match {
    case Nil => online
    case xs => offline(xs)
  }
  
  def offline(xs: List[String]) = {
    xs.foreach(x =>println(Commands.commandString(x)))  
  }
  
  def online = {
    val conf = ConfigFactory.load()
    val myScreenName = conf.getString("screenname")
    
    val client = TwitterRestClient()
    
    val now = DateTime.now
    
    client.createTweet(status = "おはよう。今日も司令を出していくわよ")
    
    Await.result(client.followersForUser(myScreenName), Duration.Inf).data.users.foreach { u =>
      val s = Commands.commandString(u.screen_name)
      client.createTweet(status = s)
    }
  }  
}

