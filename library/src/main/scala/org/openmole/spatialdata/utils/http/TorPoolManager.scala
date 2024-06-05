/**
 *
 */
package org.openmole.spatialdata.utils.http

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.InputStreamReader
import java.net.URL
import java.util.LinkedList

import scala.collection.mutable.ArrayBuffer;


/**
 * Manager communicating with the external TorPool app, via .tor_tmp files (TorPool must be run within same directory for now)
 *
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
object TorPoolManager {

	/**
	 * TODO : Concurrent access from diverse apps to a single pool ?
	 * Difficult as would need listener on this side...
	 *
	 */

	/**
	 * the port currently used.
	 */
	var currentPort: Int = 0

	var hasTorPoolConnexion: Boolean = false


	/**
	 * Checks if a pool is currently running, and setup initial port correspondingly.
	 */
	def setupTorPoolConnexion(portexclusivity: Boolean): Unit = {

		//Log.stdout("Setting up TorPool connection...");

		// check if pool is running.
		//checkRunningPool();

		if(hasRunningPool()) {

			System.setProperty("socksProxyHost", "127.0.0.1")


			try{
		 		//changePortFromFile(new BufferedReader(new FileReader(new File(".tor_tmp/ports"))));
				switchPort(portexclusivity)
			}catch{
				case e: Throwable => e.printStackTrace()
			}

			//showIP();

			hasTorPoolConnexion = true;

		}
	}


	/**
	 * Send a stop signal to the whole pool -> needed ? Yes to avoid having tasks going on running on server e.g.
	 */
	def closePool() = {}


	def checkRunningPool(): Unit = {
		if(!new File(".tor_tmp/ports").exists()){throw new Exception("NO RUNNING TOR POOL !"); }
	}

	def hasRunningPool(): Boolean = {
		if (new File(".tor_tmp").exists()){
			return(new File(".tor_tmp/ports").exists());
		}else{return(false);}
	}



	/**
	 * Switch the current port to the oldest living TorThread.
	 *   - Reads communication file -
	 */
	 def switchPort(portexclusivity: Boolean)= {
 		try{
 			//send kill signal via kill file
 			// if current port is set
 			if(currentPort!=0){
 				//Log.stdout("Sending kill signal for current tor thread...");
 				(new File(".tor_tmp/kill"+currentPort)).createNewFile()
 			}

 			val portpath = ".tor_tmp/ports"
 			val lockfile = ".tor_tmp/lock"
 			var newport = ""

 			while(newport.length()<4) {
 				if (portexclusivity) {
 					newport = readAndRemoveLineWithLock(portpath, lockfile)
 				} else {
 					newport = readLineWithLock(portpath, lockfile)
 				}
 			}

 			// show ip to check
 			showIP()

 			changePort(newport)

 		}catch{
			case e: Throwable => e.printStackTrace()
		}
 	}

 	/**
 	 *
 	 * FIXME not checked
 	 *
 	 * @param portpath
 	 * @param lockfile
 	 */
	def changePortFromFile(portpath: String,lockfile: String): String = {
 		//String newPort = "9050";
 		var newPort = ""

 		// assumes ports with 4 digits
 		// and that the file always has a content
 		while(newPort.length()<4) {
 			try{
 				newPort = readLineWithLock(portpath,lockfile);
 				if(newPort.length()<4){System.out.println("Waiting for an available tor port");Thread.sleep(1000);}
 			}catch{
				case e: Throwable => e.printStackTrace()
			}
 		}

 		// set the new port
 		System.setProperty("socksProxyPort",newPort);
 		currentPort = Integer.parseInt(newPort);
 		println("Current Port set to "+newPort);
 		newPort
 	}

 	/**
 	 *
 	 */
	def changePort(newport: String): Unit = {
 		// set the new port
 		System.setProperty("socksProxyPort",newport);
 		currentPort = Integer.parseInt(newport);
 		//Log.stdout("Current Port set to "+newport);
 	}

	/**
		 * Release the current port (without killing the task) -> used when in exclusivity ?
		 */
		def releasePort(): Unit = {
			//Log.stdout("Releasing port "+currentPort);
			switchPort(false)
		}




	/**
	 *
	 *
	 * @param portpath
	 * @param lockfile
	 * @return
	 */
	def readLineWithLock(portpath: String,lockfile: String): String = {
		var res = "";
		try{
			var locked = true
			var t=0
			while(locked){
				//Log.stdout("Waiting for lock on "+lockfile);
				Thread.sleep(200);
				locked = (new File(lockfile)).exists()
				t=t+1
			}
			val lock = new File(lockfile);lock.createNewFile();
			val r = new BufferedReader(new FileReader(new File(portpath)));
			res=r.readLine();
			lock.delete();
		}catch{
			case e: Throwable => e.printStackTrace()
		}

		res
	}

	def readAndRemoveLineWithLock(portfile: String,lockfile: String): String = {
		var res = "";
		try {
			var locked = true;
			while (locked) {
				//Log.stdout("Waiting for lock on " + lockfile);
				Thread.sleep(200)
				locked = (new File(lockfile)).exists()
			}
			val lock = new File(lockfile);lock.createNewFile()
			val r = new BufferedReader(new FileReader(new File(portfile)))
			val newcontent: ArrayBuffer[String] = new ArrayBuffer
			var currentline = r.readLine();
			while(currentline!=null){newcontent.append(currentline);currentline=r.readLine();}
			r.close();

			val w = new BufferedWriter(new FileWriter(new File(portfile)))
			res = newcontent.remove(1)
			for(remp <- newcontent){
				w.write(remp)
				w.newLine()
			}
			w.close()
			// unlock the dir
			lock.delete();
		} catch{
		  case e: Throwable => e.printStackTrace()
		}
		res
	}


	def removeInFileWithLock(s: String,file: String,lock: String): Unit = {
		try{
			var locked = true;
			while(locked){
				System.out.println("Waiting for lock on "+lock);
				Thread.sleep(200);
				locked = (new File(lock)).exists();
			}
			val lockfile = new File(lock);lockfile.createNewFile();
			val r = new BufferedReader(new FileReader(new File(file)));
			val newcontent: ArrayBuffer[String] = new ArrayBuffer
			var currentline = r.readLine();
			while(currentline!=null){
				if(currentline.replace("\n","")!=s){
					newcontent.append(currentline)
					currentline=r.readLine()}
			}
			r.close()
			val w = new BufferedWriter(new FileWriter(new File(file)))
			for(remp <- newcontent){
				w.write(remp)
				w.newLine()
			}
			w.close()
			// unlock the dir
			lockfile.delete();
		}catch{
			case e: Throwable => e.printStackTrace()
		}
	}




	// Test functions
/*
	private static void testRemotePool(){
		try{setupTorPoolConnexion();

		showIP();

		while(true){
			Thread.sleep(10000);
			Log.stdout("TEST : Switching port... ");
			switchPort();
			showIP();
		}
		}catch(Exception e){e.printStackTrace();}
	}
	*/

	def showIP(): Unit = {
		try{
		val r = new BufferedReader(new InputStreamReader(new URL("http://ipecho.net/plain").openConnection().getInputStream()));
		var currentLine=r.readLine();
		while(currentLine!= null){
			//Log.stdout(currentLine);
			currentLine=r.readLine();}
		}catch{
			case e: Throwable => e.printStackTrace()
		}
	}







}
