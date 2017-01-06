// SoftRains:  a Genuine People Personality for your home
// Copyright 2016 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package softrains.vision

import softrains.base._

import java.io._
import java.nio._
import java.net._
import java.nio.file._
import java.text._
import java.util._

import java.awt._
import java.awt.event._

import javax.swing._

import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.helper.opencv_core._
import org.bytedeco.javacpp.opencv_face._
import org.bytedeco.javacpp.opencv_video._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_objdetect._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.avutil._
import org.bytedeco.javacpp._
import org.bytedeco.javacv._
import org.bytedeco.javacv.{Frame => CvFrame}

import scala.collection._

object UrlAuthHandler extends sun.net.www.protocol.http.Handler
{
  override def openConnection(url : URL) =
  {
    val connection = super.openConnection(url)
    Option(url.getUserInfo) match {
      case Some(userInfo) => {
        val basicAuth = "Basic " +
          javax.xml.bind.DatatypeConverter.printBase64Binary(userInfo.getBytes)
        connection.setRequestProperty("Authorization", basicAuth);
      }
      case _ =>
    }
    connection
  }
}

trait CameraView
{
  def display(frame : CvFrame)

  def quit()

  def isClosed() = false
}

trait CameraInput
{
  protected var frameGrabber : Option[FrameGrabber] = None

  protected def newGrabber : FrameGrabber

  private var closed = false

  def startGrabber()
  {
    val grabber = newGrabber
    grabber.setBitsPerPixel(CV_8U)
    grabber.setImageMode(FrameGrabber.ImageMode.COLOR)
    grabber.start
    frameGrabber = Some(grabber)
  }

  def stopGrabber()
  {
    frameGrabber.foreach(_.stop)
    frameGrabber = None
  }

  def nextFrame() : Option[CvFrame] =
  {
    val next = frameGrabber.flatMap(grabber => Option(grabber.grab))
    if (next.isEmpty) {
      closed = true
    }
    next
  }

  def isClosed() = closed
}

class VideoFileInput(file : File)
    extends CameraInput
{
  override protected def newGrabber =
  {
    av_log_set_level(AV_LOG_QUIET)
    new FFmpegFrameGrabber(file)
  }
}

class CameraFeedInput(feedUrl : String)
    extends CameraInput
{
  private val url = new URL(new URL(feedUrl), "videofeed", UrlAuthHandler)

  override protected def newGrabber =
  {
    // see https://github.com/bytedeco/javacv/issues/411
    new IPCameraFrameGrabber(
      url, -1, -1, java.util.concurrent.TimeUnit.SECONDS)
  }

  override def nextFrame() =
  {
    stopGrabber
    startGrabber
    super.nextFrame
  }
}

class CameraLocalInput
    extends CameraInput
{
  override def nextFrame() =
  {
    stopGrabber
    startGrabber
    super.nextFrame
  }

  override protected def newGrabber =
  {
    val grabber = new OpenCVFrameGrabber(0)
    grabber
  }
}

object CameraNullView extends CameraView
{
  override def display(frame : CvFrame)
  {}

  override def quit()
  {}
}

class CameraDesktopView(feedName : String) extends CameraView
{
  private val canvasFrame = initCanvasFrame

  private var closed = false

  private def initCanvasFrame() =
  {
    val cf = new CanvasFrame(feedName)
    cf.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    cf.addWindowListener(new WindowAdapter {
      override def windowClosing(e : WindowEvent)
      {
        super.windowClosing(e)
        closed = true
      }

      override def windowClosed(e : WindowEvent)
      {
        super.windowClosed(e)
      }
    })
    cf
  }

  override def isClosed() = closed

  override def display(frame : CvFrame)
  {
    canvasFrame.showImage(frame)
  }

  override def quit()
  {
    Toolkit.getDefaultToolkit.getSystemEventQueue.postEvent(
      new WindowEvent(canvasFrame, WindowEvent.WINDOW_CLOSING))
  }
}

trait RecordingProducer
{
  private val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")

  private val timeFormatter = new SimpleDateFormat("HHmmss")

  protected var recordingDirOpt : Option[File] = None

  protected def getRecordingDir = recordingDirOpt.get

  protected def getTimestamp = Calendar.getInstance.getTime

  protected def getDateDir(timestamp : Date) : File =
  {
    val dateStamp = dateFormatter.format(timestamp)
    new File(getRecordingDir, "d" + dateStamp)
  }

  protected def getTimeFile(
    timestamp : Date, prefix : String, suffix : String) : File =
  {
    val timeStamp = timeFormatter.format(timestamp)
    new File(getDateDir(timestamp), prefix + timeStamp + "." + suffix)
  }

  protected def getFacesDir : File = new File(getRecordingDir, "faces")
}

class CameraSentinel(
  input : CameraInput, view : CameraView, settings : SoftRainsSettings)
    extends RecordingProducer
{
  // workaround, may be able to eliminate after upgrade to javacv 1.3
  Loader.load(classOf[opencv_objdetect])

  private val recorder = new VideoRecorder

  private val bgSubtractor = createBackgroundSubtractorMOG2(200, 130, false)

  private val blobMergeDistance = settings.Visitors.blobMergeDistance

  private val blobMinSize = settings.Visitors.blobMinSize

  private val bodyMinSize = settings.Visitors.bodyMinSize

  private val visitorProximityZone = settings.Visitors.proximityZone

  private var detectFaces = false

  private var saveFaces = false

  private var detectVisitors = false

  private var pareidolia = false

  private var faceRecognizerOpt : Option[FaceRecognizer] = None

  private val faceLabels = new mutable.HashMap[String, Int]

  private var faceLabelsInv : mutable.Map[Int, String] = null

  private var proximityDetected = false

  private var visitorDetected = false

  private var faceDetected = false

  private var lastMotion = 0

  private var frameAnalyzerOpt : Option[FrameAnalyzer] = None

  def wasVisitorDetected = visitorDetected

  def wasFaceDetected = faceDetected

  private var recordMotion = false

  private var lastFace = ""

  private var faceConfidence = 0.0

  def getLastFace = lastFace

  def getFaceConfidence = faceConfidence

  private def createDirs()
  {
    val recordingDir = settings.Files.videoPath
    if (!recordingDir.isDirectory) {
      if (!recordingDir.mkdirs) {
        throw new IOException(
          "Unable to create recording directory " + recordingDir)
      }
    }
    recordingDirOpt = Some(recordingDir)
    val facesDir = getFacesDir
    if (!facesDir.isDirectory) {
      if (!facesDir.mkdirs) {
        throw new IOException(
          "Unable to create faces directory " + facesDir)
      }
    }
  }

  def enableMotionRecording()
  {
    recordMotion = true
    createDirs
  }

  def disableFaceDetection()
  {
    detectFaces = false
    faceDetected = false
  }

  def inducePareidolia()
  {
    pareidolia = true
  }

  def enableFaceDetection(save : Boolean)
  {
    detectFaces = true
    saveFaces = save
    createDirs
    if (!settings.Visitors.subConf.getString("training-path").isEmpty) {
      trainFaceRecognition
    }
  }

  def enableVisitorDetection()
  {
    detectVisitors = true
  }

  private def quit()
  {
    input.stopGrabber
    view.quit
    recorder.quit
  }

  private def trainFaceRecognition()
  {
    val root = settings.Visitors.trainingPath
    val imgFilter = new FilenameFilter {
      override def accept(dir : File, nameOrig : String) =
      {
        val name = nameOrig.toLowerCase
        name.endsWith(".jpg") || name.endsWith(".pgm") || name.endsWith(".png")
      }
    }

    val imageFiles = root.listFiles(imgFilter)
    val images = new MatVector(2*imageFiles.length)
    val labels = new Mat(2*imageFiles.length, 1, CV_32SC1)
    val labelsBuf = labels.createBuffer[IntBuffer]

    var counter = 0

    for (image <- imageFiles) {
      val img = imread(image.getAbsolutePath, CV_LOAD_IMAGE_GRAYSCALE)
      val labelString = image.getName.split("\\-")(0)
      val labelOpt = faceLabels.get(labelString)
      val label = labelOpt match {
        case Some(l) => l
        case _ => {
          val newLabel = faceLabels.size
          faceLabels.put(labelString, newLabel)
          newLabel
        }
      }
      images.put(counter, img)
      labelsBuf.put(counter, label)
      counter += 1

      val copy = new IplImage(img).clone
      cvFlip(copy, copy, 1)
      images.put(counter, new Mat(copy))
      labelsBuf.put(counter, label)
      counter += 1
    }
    faceLabelsInv = faceLabels.map(_.swap)

    val faceRecognizer = createLBPHFaceRecognizer
    faceRecognizer.train(images, labels)
    faceRecognizerOpt = Some(faceRecognizer)

    for (i <- 0 until images.size.toInt) {
      images.get(i).release
    }
  }

  private def loadClassifier(classifierName : String) =
  {
    val classifierFile = new File("vision/data", classifierName)
    if ((classifierFile == null) || (classifierFile.length <= 0)) {
      throw new IOException(
        "Classifier " + classifierFile.getAbsolutePath + " not found.")
    }
    val classifier = cvLoadHaarClassifierCascade(
      classifierFile.getCanonicalPath, cvSize(0, 0))
    if ((classifier == null) || classifier.isNull) {
      throw new IOException("Could not load classifier " + classifierName)
    }
    classifier
  }

  private def newFrameConverter = new OpenCVFrameConverter.ToIplImage

  private def convert(img : IplImage) : CvFrame =
  {
    newFrameConverter.convert(img)
  }

  private def convert(frame : CvFrame) : IplImage =
  {
    newFrameConverter.convert(frame)
  }

  private def applyClassifier(
    classifier : CvHaarClassifierCascade,
    storage : CvMemStorage,
    gray : IplImage,
    minSize : Int = 0) : Seq[CvRect] =
  {
    cvClearMemStorage(storage)
    val seq = cvHaarDetectObjects(
      gray, classifier, storage, 1.1, 10,
      CV_HAAR_FIND_BIGGEST_OBJECT | CV_HAAR_DO_ROUGH_SEARCH,
      cvSize(minSize, minSize), cvSize(0, 0))
    if (seq == null) {
      Seq.empty
    } else {
      (0 until seq.total).map(
        i => new CvRect(cvGetSeqElem(seq, i)))
    }
  }

  def run()
  {
    startAnalyzer
    try {
      while (!view.isClosed && !input.isClosed) {
        analyzeFrame
      }
    } finally {
      stopAnalyzer
    }
  }

  def startAnalyzer()
  {
    assert(frameAnalyzerOpt.isEmpty)
    input.startGrabber
    frameAnalyzerOpt = Some(new FrameAnalyzer)
  }

  def stopAnalyzer()
  {
    assert(!frameAnalyzerOpt.isEmpty)
    frameAnalyzerOpt.foreach(_.release)
    quit
  }

  def analyzeFrame()
  {
    assert(!frameAnalyzerOpt.isEmpty)
    if (detectVisitors || detectFaces || recordMotion || settings.Test.active) {
      frameAnalyzerOpt.foreach(_.analyzeFrame)
    }
  }

  private class FrameAnalyzer
  {
    val bodyClassifier = loadClassifier("haarcascade_upperbody.xml")
    val faceClassifier = loadClassifier("haarcascade_frontalface_alt.xml")
    val bodyStorage = AbstractCvMemStorage.create
    val contourStorage = AbstractCvMemStorage.create
    val faceStorage = AbstractCvMemStorage.create
    val cropped = AbstractIplImage.create(new CvSize(100, 100), 8, 1)
    var diffOpt : Option[IplImage] = None
    var grayOpt : Option[IplImage] = None

    def analyzeFrame()
    {
      input.nextFrame match {
        case Some(frame) => {
          val img = convert(frame)
          if (grayOpt.isEmpty) {
            grayOpt = Some(AbstractIplImage.create(cvGetSize(img), 8, 1))
          }
          val gray = grayOpt.get
          cvCvtColor(img, gray, CV_BGR2GRAY)
          cvSmooth(gray, gray, CV_GAUSSIAN, 3, 3, 0, 0)
          val blobMinPixels = img.height * blobMinSize
          if (recordMotion || detectVisitors) {
            val first = diffOpt.isEmpty
            if (first) {
              diffOpt = Some(AbstractIplImage.create(
                img.width, img.height, IPL_DEPTH_8U, 1))
            }
            val diff = diffOpt.get
            cvZero(diff)
            bgSubtractor.apply(new Mat(gray), new Mat(diff), -1)
            cvErode(diff, diff, null, 3)
            val nonZeroCount = countNonZero(new Mat(diff))
            if (!first && (nonZeroCount > 10)) {
              cvClearMemStorage(contourStorage)
              var contour = new CvSeq(null)
              cvFindContours(
                diff, contourStorage, contour,
                Loader.sizeof(classOf[CvContour]),
                CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0))
              val rects = new mutable.ArrayBuffer[CvRect]
              while ((contour != null) && !contour.isNull) {
                if (contour.elem_size > 0) {
                  val rect = cvBoundingRect(contour)
                  rects += rect
                }
                contour = contour.h_next
              }
              val maxDistance = img.height * blobMergeDistance
              val blobMerger = new BlobProximityMerger(maxDistance.toFloat)
              val blobs = blobMerger.merge(rects).filter(blob =>
                (blob.width > blobMinPixels) && (blob.height > blobMinPixels))
              val proximityThreshold =
                img.height - (img.height * visitorProximityZone)
              def isCloseEnough(blob : CvRect) =
                ((blob.y + blob.height) > proximityThreshold)
              if (recordMotion) {
                if (!blobs.isEmpty) {
                  lastMotion = 0
                  recorder.enableRecording(recordingDirOpt)
                }
                recorder.store(frame)
              }
              if (detectVisitors) {
                if (blobs.exists(isCloseEnough(_))) {
                  proximityDetected = true
                }
                if (proximityDetected) {
                  val bodyMinPixels = bodyMinSize * img.height
                  val bodies = applyClassifier(
                    bodyClassifier, bodyStorage, gray, bodyMinPixels.toInt)
                  if (!bodies.isEmpty) {
                    visitorDetected = true
                  }
                  if (detectFaces) {
                    blobs.foreach(visitor => {
                      detectFacesInRegion(
                        img, gray, visitor, blobMinPixels.toInt)
                    })
                  }
                  bodies.foreach(
                    body => highlightRectangle(
                      img, body, AbstractCvScalar.CYAN)
                  )
                }
                blobs.foreach(blob => {
                  highlightRectangle(
                    img, blob,
                    if (isCloseEnough(blob)) {
                      AbstractCvScalar.RED
                    } else {
                      AbstractCvScalar.GREEN
                    }
                  )
                })
              }
            }
          } else if (detectFaces) {
            faceDetected = false
            lastFace = ""
            val region = new CvRect(0, 0, img.width, img.height)
            detectFacesInRegion(img, gray, region, blobMinPixels.toInt)
          }
          view.display(frame)
          img.release
          if (recordMotion) {
            lastMotion += 1
            if (lastMotion > 6) {
              endRecording
            }
          }
        }
        case _ =>
      }
    }

    private def detectFacesInRegion(
      img : IplImage, gray : IplImage, region : CvRect, blobMinPixels : Int)
    {
      cvSetImageROI(gray, region)
      var faces = applyClassifier(
        faceClassifier, faceStorage, gray, blobMinPixels)
      if (pareidolia && faces.isEmpty) {
        faces = Seq(region)
      }
      cvResetImageROI(gray)

      faceRecognizerOpt match {
        case Some(faceRecognizer) => {
          faces = faces.filter(face => {
            cvSetImageROI(gray, nestRect(region, face))
            cvResize(gray, cropped)
            val pLabel = new IntPointer(1L)
            val pConfidence = new DoublePointer(1L)
            faceRecognizer.predict(new Mat(cropped), pLabel, pConfidence)
            val predicted = pLabel.get
            faceConfidence = pConfidence.get
            cvResetImageROI(gray)
            if (faceConfidence > 150.0) {
              false
            } else {
              lastFace = faceLabelsInv.get(predicted).getOrElse("stranger")
              true
            }
          })
        }
        case _ =>
      }

      if (!faces.isEmpty) {
        visitorDetected = true
        faceDetected = true
      }

      if (saveFaces) {
        faces.foreach(
          face => {
            cvSetImageROI(img, nestRect(region, face))
            val outFileName = File.createTempFile(
              "face", ".jpg", getFacesDir).getCanonicalPath
            cvSaveImage(outFileName, img)
            cvResetImageROI(img)
          }
        )
      }

      cvSetImageROI(img, region)
      faces.foreach(face => {
        val color = AbstractCvScalar.BLUE
        highlightRectangle(
          img, face, color)
      })
      cvResetImageROI(img)
    }

    def release()
    {
      cropped.release
      faceStorage.release
      bodyStorage.release
      contourStorage.release
      diffOpt.foreach(_.release)
      grayOpt.foreach(_.release)
    }
  }

  def endRecording()
  {
    if (visitorDetected) {
      recorder.storeVisitorDetected()
    }
    if (faceDetected) {
      recorder.storeFaceDetected()
    }
    recorder.enableRecording(None)
    recorder.quit
    lastFace = ""
    faceDetected = false
    visitorDetected = false
    proximityDetected = false
  }

  private def nestRect(outerRect : CvRect, innerRect : CvRect) : CvRect =
    new CvRect(
      outerRect.x + innerRect.x,
      outerRect.y + innerRect.y,
      innerRect.width,
      innerRect.height)

  private def highlightRectangle(
    img : IplImage, rect : CvRect, color : CvScalar)
  {
    cvRectangle(
      img,
      cvPoint(rect.x, rect.y),
      cvPoint(rect.x + rect.width, rect.y + rect.height),
      color,
      2, 8, 0)
  }
}

class VideoRecorder(filterString : String = "")
    extends RecordingProducer
{
  private var recorder : Option[FrameRecorder] = None

  private var filter : Option[FrameFilter] = None

  private var recordingFile : Option[File] = None

  private var visitorDetectedFile : Option[File] = None

  private var faceDetectedFile : Option[File] = None

  private def isEnabled = !recordingDirOpt.isEmpty

  def enableRecording(dirOpt : Option[File])
  {
    recordingDirOpt = dirOpt
    if (!isEnabled) {
      recordingFile = None
      visitorDetectedFile = None
      faceDetectedFile = None
    }
  }

  private def initRecorder(firstFrame : CvFrame) =
  {
    val width = firstFrame.imageWidth
    val height = firstFrame.imageHeight
    if (!filterString.isEmpty) {
      val f = new FFmpegFrameFilter(filterString, width, height)
      f.start
      filter = Some(f)
    }
    val suffix = "mkv"
    val timestamp = Calendar.getInstance.getTime
    val dateDir = getDateDir(timestamp)
    if (!dateDir.isDirectory) {
      if (!dateDir.mkdirs) {
        throw new IOException(
          "Unable to create daily recording directory " + dateDir)
      }
    }
    val file = getTimeFile(timestamp, "r", suffix)
    recordingFile = Some(file)
    visitorDetectedFile = Some(getTimeFile(timestamp, "v", suffix))
    faceDetectedFile = Some(getTimeFile(timestamp, "f", suffix))
    val r = new FFmpegFrameRecorder(file, width, height)
    r.setFormat(suffix)
    r.setFrameRate(4)
    r.setPixelFormat(AV_PIX_FMT_YUV420P)
    r.start
    r
  }

  def storeFaceDetected()
  {
    faceDetectedFile.foreach(file =>
      Files.createSymbolicLink(file.toPath, recordingFile.get.toPath))
  }

  def storeVisitorDetected()
  {
    visitorDetectedFile.foreach(file =>
      Files.createSymbolicLink(file.toPath, recordingFile.get.toPath))
  }

  def store(frame : CvFrame)
  {
    if (!isEnabled) {
      return
    }
    if (recorder.isEmpty) {
      recorder = Some(initRecorder(frame))
    }
    recorder.foreach(r => {
      filter match {
        case Some(f) => {
          f.push(frame)
          var filteredFrame : Option[CvFrame] = None
          do {
            filteredFrame = Option(f.pull)
            filteredFrame.foreach(r.record(_))
          } while (!filteredFrame.isEmpty)
            }
        case _ => {
          r.record(frame)
        }
      }
    })
  }

  def quit()
  {
    this.synchronized {
      recorder.foreach(r => {
        r.stop
        r.release
      })
      filter.foreach(f => {
        f.stop
        f.release
      })
      recorder = None
      filter = None
    }
  }
}
