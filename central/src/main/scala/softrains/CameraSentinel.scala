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
package softrains

import java.io._
import java.net._
import java.text._
import java.util._

import java.awt._
import java.awt.event._

import javax.swing._

import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.helper.opencv_core._
import org.bytedeco.javacpp.opencv_video._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_objdetect._
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

class CameraFileInput(file : File)
    extends CameraInput
{
  override protected def newGrabber =
  {
    av_log_set_level(AV_LOG_QUIET)
    new FFmpegFrameGrabber(file)
  }
}

class CameraFeedInput(feed : CameraFeed)
    extends CameraInput
{
  private val url = new URL(new URL(feed.url), "videofeed", UrlAuthHandler)

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

object CameraNullView extends CameraView
{
  override def display(frame : CvFrame)
  {}

  override def quit()
  {}
}

class CameraDesktopView(feed : CameraFeed) extends CameraView
{
  private val canvasFrame = initCanvasFrame

  private var closed = false

  private def initCanvasFrame() =
  {
    val cf = new CanvasFrame(feed.name)
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

class CameraSentinel(
  input : CameraInput, view : CameraView, settings : CentralSettings)
{
  private val recorder = new VideoRecorder

  private val bgSubtractor = createBackgroundSubtractorMOG2(200, 130, false)

  private val blobMergeDistance = settings.Visitors.blobMergeDistance

  private val blobMinSize = settings.Visitors.blobMinSize

  private val bodyMinSize = settings.Visitors.bodyMinSize

  private val visitorProximityZone = settings.Visitors.proximityZone

  private var saveFaces = false

  private var detectVisitors = false

  private var recordingDirOpt : Option[File] = None

  private var proximityDetected = false

  private var visitorDetected = false

  private var faceDetected = false

  private var nextFaceNumber = 0

  private var lastMotion = 0

  def wasVisitorDetected = visitorDetected

  def wasFaceDetected = faceDetected

  private def recordMotion = !recordingDirOpt.isEmpty

  def enableMotionRecording()
  {
    val recordingDir = settings.Files.videoPath
    if (!recordingDir.isDirectory) {
      if (!recordingDir.mkdirs) {
        throw new IOException(
          "Unable to create recording directory " + recordingDir)
      }
    }
    recordingDirOpt = Some(recordingDir)
  }

  def enableVisitorDetection(save : Boolean)
  {
    detectVisitors = true
    saveFaces = save
  }

  private def quit()
  {
    input.stopGrabber
    view.quit
    recorder.quit
  }

  private def loadClassifier(classifierName : String) =
  {
    val classifierFile = new File("data", classifierName)
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
    input.startGrabber
    val bodyClassifier = loadClassifier("haarcascade_upperbody.xml")
    val faceClassifier = loadClassifier("haarcascade_frontalface_alt.xml")
    val bodyStorage = AbstractCvMemStorage.create
    val contourStorage = AbstractCvMemStorage.create
    val faceStorage = AbstractCvMemStorage.create
    var diffOpt : Option[IplImage] = None
    var grayOpt : Option[IplImage] = None
    try {
      while (!view.isClosed && !input.isClosed) {
        input.nextFrame match {
          case Some(frame) => {
            val img = convert(frame)
            if (grayOpt.isEmpty) {
              grayOpt = Some(AbstractIplImage.create(cvGetSize(img), 8, 1))
            }
            val gray = grayOpt.get
            cvCvtColor(img, gray, CV_BGR2GRAY)
            cvSmooth(gray, gray, CV_GAUSSIAN, 3, 3, 0, 0)
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
                val blobMinPixels = img.height * blobMinSize
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
                    blobs.foreach(visitor => {
                      cvSetImageROI(gray, visitor)
                      val faces = applyClassifier(
                        faceClassifier, faceStorage, gray, blobMinPixels.toInt)
                      cvResetImageROI(gray)
                      if (!faces.isEmpty) {
                        visitorDetected = true
                        faceDetected = true
                      }
                      if (saveFaces) {
                        faces.foreach(
                          face => {
                            cvSetImageROI(img, nestRect(visitor, face))
                            val outFileName = "/tmp/face" +
                            nextFaceNumber + ".jpg"
                            nextFaceNumber += 1
                            cvSaveImage(outFileName, img)
                            cvResetImageROI(img)
                          }
                        )
                      }
                      cvSetImageROI(img, visitor)
                      faces.foreach(
                        face => highlightRectangle(
                          img, face, AbstractCvScalar.BLUE)
                      )
                      cvResetImageROI(img)
                    })
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
            }
            view.display(frame)
            img.release
            if (recordMotion) {
              lastMotion += 1
              if (lastMotion > 6) {
                if (visitorDetected) {
                  recorder.storeVisitorDetected()
                }
                if (faceDetected) {
                  recorder.storeFaceDetected()
                }
                recorder.enableRecording(None)
                recorder.quit
                faceDetected = false
                visitorDetected = false
                proximityDetected = false
              }
            }
          }
          case _ =>
        }
      }
    } finally {
      faceStorage.release
      bodyStorage.release
      contourStorage.release
      diffOpt.foreach(_.release)
      grayOpt.foreach(_.release)
      quit
    }
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
{
  private var recorder : Option[FrameRecorder] = None

  private var filter : Option[FrameFilter] = None

  private var recordingDirOpt : Option[File] = None

  private var visitorDetectedFile : Option[File] = None

  private var faceDetectedFile : Option[File] = None

  private def isEnabled = !recordingDirOpt.isEmpty

  private val sdfFilename = new SimpleDateFormat("yyyy-MM-dd'T'HHmmss")

  def enableRecording(dirOpt : Option[File])
  {
    recordingDirOpt = dirOpt
    if (!isEnabled) {
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
    val timestamp = sdfFilename.format(Calendar.getInstance.getTime)
    val recordingDir = recordingDirOpt.get
    val file = new File(recordingDir, "r" + timestamp + "." + suffix)
    visitorDetectedFile = Some(new File(recordingDir, "v" + timestamp + ".txt"))
    faceDetectedFile = Some(new File(recordingDir, "f" + timestamp + ".txt"))
    val r = new FFmpegFrameRecorder(file, width, height)
    r.setFormat(suffix)
    r.setFrameRate(4)
    r.setPixelFormat(AV_PIX_FMT_YUV420P)
    r.start
    r
  }

  def storeFaceDetected()
  {
    faceDetectedFile.foreach(_.createNewFile)
  }

  def storeVisitorDetected()
  {
    visitorDetectedFile.foreach(_.createNewFile)
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
