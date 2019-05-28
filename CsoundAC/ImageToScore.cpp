/*
* C S O U N D
*
* L I C E N S E
*
* This software is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This software is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this software; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifdef _MSC_VER
#define NOMINMAX
#endif
#include "CppSound.hpp"
#include "ImageToScore.hpp"
#include "System.hpp"
#include <FL/Fl_Image.H>
#include <FL/Fl_PNM_Image.H>
#include <FL/Fl_XPM_Image.H>
#include <FL/Fl_GIF_Image.H>
///#include <FL/Fl_BMP_Image.H>
#include <FL/Fl_PNG_Image.H>
#include <FL/Fl_JPEG_Image.H>
#include <cmath>
#include <complex>
#include <set>
#include <functional>
#include <opencv2/imgcodecs.hpp>

namespace csound
{

ImageToScore::ImageToScore(void) : image(0), maximumVoiceCount(4), minimumValue(0)
{
}

ImageToScore::~ImageToScore(void)
{
}

void ImageToScore::setImageFilename(std::string imageFilename)
{
    this->imageFilename = imageFilename;
}

std::string ImageToScore::getImageFilename() const
{
    return imageFilename;
}

void ImageToScore::translate(double x, double y, double hue, double value, Event &event) const
{
    event[Event::STATUS] = 144;
    event[Event::TIME] = ((x / double(image->w())) * score.scaleTargetRanges[Event::TIME]) + score.scaleTargetMinima[Event::TIME];
    event[Event::INSTRUMENT] = (hue * score.scaleTargetRanges[Event::INSTRUMENT]) + score.scaleTargetMinima[Event::INSTRUMENT];
    event[Event::KEY] = int((((image->h() - y) / double(image->h())) * score.scaleTargetRanges[Event::KEY]) + score.scaleTargetMinima[Event::KEY] + 0.5);
    event[Event::VELOCITY] = (value * score.scaleTargetRanges[Event::VELOCITY]) + score.scaleTargetRanges[Event::VELOCITY];
    // static char buffer[0x100];
    // sprintf(buffer, "Translate: x %d y %d  %s\n", int(x), int(y), event.toString().c_str());
}

void ImageToScore::rgbToHsv(double r, double g, double b, double &h, double &s, double &v)
{
    double maxc = std::max(std::max(r, g), b);
    double minc = std::min(std::min(r, g), b);
    v = maxc;
    if (minc == maxc)
    {
        h = 0;
        s = 0;
        return;
    }
    s = (maxc-minc) / maxc;
    double rc = (maxc-r) / (maxc-minc);
    double gc = (maxc-g) / (maxc-minc);
    double bc = (maxc-b) / (maxc-minc);
    if (r == maxc)
    {
        h = bc-gc;
    }
    else if( g == maxc)
    {
        h = 2.0+rc-bc;
    }
    else
    {
        h = 4.0+gc-rc;
    }
    h = std::fmod(h/6.0, 1.0);
}

void ImageToScore::getPixel(size_t x, size_t y, double &hue, double &saturation, double &value) const
{
    if(image) {
        size_t index = (y * image->w() * image->d()) + (x * image->d());
        const unsigned char *data = (const unsigned char *)image->data()[0];
        double red   = double(*(data + index + 0));
        double green = double(*(data + index + 1));
        double blue  = double(*(data + index + 2));
        rgbToHsv(red / 255.0, green / 255.0, blue / 255.0, hue, saturation, value);
        // System::debug("x=%5d y=%5d  r=%5.1f g=%5.1f b=%5.1f  h=%7.3f s=%7.3f v=%7.3f\n", x, y, red, green, blue, hue, saturation, value);
    }
}

void ImageToScore::setMaximumVoiceCount(size_t maximumVoiceCount)
{
    this->maximumVoiceCount = maximumVoiceCount;
}

size_t ImageToScore::getMaximumVoiceCount() const
{
    return maximumVoiceCount;
}

void ImageToScore::setMinimumValue(double minimumValue)
{
    this->minimumValue = minimumValue;
}

double ImageToScore::getMinimumValue() const
{
    return minimumValue;
}

/**
* Generate a score with up to N voices,
* based on the pixels in an image file.
* The horizontal (x) dimension is time,
* the vertical (y) dimension is pitch (MIDI key),
* the value (brightness or v) of the pixel is loudness (MIDI velocity),
* and the hue (color or h) of the pixel is instrument number.
* The translation algorithm is:
* for each column of pixels,
* compare the value of the pixel in each row
* to the value in the previous column
* and the value in the next column.
* If the current value is greater than the previous value or some minimum value V,
* begin an event for the pitch of that row;
* if the current value is less than V, end the event;
* otherwise, continue the event.
*/
void ImageToScore::generate()
{
    System::inform("BEGAN ImageToScore::generate()...\n");
    if (image) {
        delete image;
        image = 0;
    }
    if (imageFilename.find(".jpg") != std::string::npos || imageFilename.find(".JPG") != std::string::npos ||
            imageFilename.find(".jpeg") != std::string::npos || imageFilename.find(".JPEG") != std::string::npos) {
        image = new Fl_JPEG_Image(imageFilename.c_str());
    } else if (imageFilename.find(".png") != std::string::npos || imageFilename.find(".PNG") != std::string::npos) {
        image = new Fl_PNG_Image(imageFilename.c_str());
    } else if (imageFilename.find(".gif") != std::string::npos || imageFilename.find(".GIF") != std::string::npos) {
        image = new Fl_GIF_Image(imageFilename.c_str());
        //} else if(imageFilename.find(".xpm") != std::string::npos || imageFilename.find(".XPM") != std::string::npos) {
        //    image = new Fl_XPM_Image(imageFilename.c_str());
    } else if(imageFilename.find(".pnm") != std::string::npos || imageFilename.find(".PNM") != std::string::npos) {
        image = new Fl_PNM_Image(imageFilename.c_str());
    ///} else if(imageFilename.find(".bmp") != std::string::npos || imageFilename.find(".BMP") != std::string::npos) {
    ///    image = new Fl_BMP_Image(imageFilename.c_str());
    } else {
        System::error("Image file '%s' not found, or unsupported image format.\n", getImageFilename().c_str());
    }
    if (!image) {
        System::error("Failed to load mage.\n");
        System::inform("ENDED ImageToScore::generate().\n");
        return;
    }
    System::inform("Loaded image file \"%s\".\n", imageFilename.c_str());
    double x = 0.0;
    double y = 0.0;
    double w = image->w();
    double h = image->h();
    System::inform("Image width  = %d\n", int(w));
    System::inform("Image height = %d\n", int(h));
    System::inform("Image depth  = %d\n", image->d());
    System::inform("Image count  = %d\n", image->count());
    if (image->d() < 3) {
        System::error("Image must have depth 3 or greater.\n");
        System::inform("ENDED ImageToScore::generate().\n");
        return;
    }
    double saturation;
    double previousValue;
    double previousHue;
    double currentValue;
    double currentHue;
    double nextValue;
    double nextHue;
    Event startingEvent;
    Event endingEvent;
    // Index is int(velocity) * 1000 + channel.
    std::map<int, Event> startingEvents;
    // Index is int(key).
    std::map<int, Event> pendingEvents;
    for (x = 0.0; x < w; ++x) {
        System::debug("Column %5d\n", int(x));
        startingEvents.clear();
        // Detect starting events.
        for (y = 0; y < h; ++y) {
            getPixel(size_t(x), size_t(y), currentHue, saturation, currentValue);
            if (x == 0.0) {
                previousHue = 0.0;
                previousValue = 0.0;
            } else {
                getPixel(size_t(x - 1.0), size_t(y), previousHue, saturation, previousValue);
            }
            if (currentValue >= minimumValue && previousValue < minimumValue) {
                translate(x, y, currentHue, currentValue, startingEvent);
                System::debug("Starting event at  (x =%5d, y =%5d, value = %8.2f): %s\n", size_t(x), size_t(y), currentValue, startingEvent.toString().c_str());
                int startingEventsIndex = int(startingEvent.getVelocityNumber() * 1000.0) + int(startingEvent.getChannel());
                startingEvents[startingEventsIndex] = startingEvent;
            }
        }
        // Insert starting events into the pending event list, in order of decreasing loudness,
        // until the pending event list has no more than maximumCount events.
        for (std::map<int, Event>::reverse_iterator startingEventsIterator = startingEvents.rbegin();
                startingEventsIterator != startingEvents.rend();
                ++startingEventsIterator) {
            if (pendingEvents.size() < maximumVoiceCount) {
                int pendingEventIndex = startingEventsIterator->second.getKeyNumber();
                // Do not interrupt an existing event.
                if (pendingEvents.find(pendingEventIndex) == pendingEvents.end()) {
                    System::debug("Pending event at   (x =%5d, y =%5d, value = %8.2f): %s\n", size_t(x), size_t(y), currentValue, startingEventsIterator->second.toString().c_str());
                    pendingEvents[pendingEventIndex] = startingEventsIterator->second;
                }
            } else {
                break;
            }
        }
        // Remove ending events from the pending event list and put them in the score.
        for (y = 0.0; y < h; ++y) {
            getPixel(size_t(x), size_t(y), currentHue, saturation, currentValue);
            if (x < (w - 1.0)) {
                getPixel(size_t(x + 1.0), size_t(y), nextHue, saturation, nextValue);
            } else {
                nextValue = 0;
                nextHue = 0;
            }
            if (currentValue >= minimumValue && nextValue < minimumValue) {
                translate(x, y, nextHue, nextValue, endingEvent);
                System::debug("Ending event at    (x =%5d, y =%5d, value = %8.2f): %s\n", size_t(x), size_t(y), currentValue, endingEvent.toString().c_str());
                int pendingEventIndex = endingEvent.getKeyNumber();
                if (pendingEvents.find(pendingEventIndex) != pendingEvents.end()) {
                    Event &startingEvent = pendingEvents[pendingEventIndex];
                    startingEvent.setDuration(endingEvent.getTime() - startingEvent.getTime());
                    if (startingEvent.getDuration() > 0.0) {
                        score.push_back(startingEvent);
                        System::inform("Inserting event at (x =%5d, y =%5d):                   %s\n", size_t(x), size_t(y), startingEvent.toString().c_str());
                        System::inform("Events pending=        %5d\n", pendingEvents.size());
                    }
                }
                pendingEvents.erase(pendingEventIndex);
            }
        }
    }
    System::debug("Remaining events...\n");
    // Insert any remaining events.
    double endingTime = score.scaleTargetMinima[Event::TIME] + score.scaleTargetRanges[Event::TIME];
    for (std::map<int, Event>::iterator it = pendingEvents.begin(); it != pendingEvents.end(); ++it) {
        startingEvent = it->second;
        startingEvent[Event::DURATION] = endingTime - startingEvent[Event::TIME];
        score.push_back(startingEvent);
        System::debug("Ending:   %s\n", startingEvent.toString().c_str());
    }
    System::inform("ENDED ImageToScore::generate().\n");
}

ImageToScore2::ImageToScore2(void) {
}

ImageToScore2::~ImageToScore2(void) {
}

void ImageToScore2::setImageFilename(std::string image_filename_) {
    image_filename = image_filename_;
}

std::string ImageToScore2::getImageFilename() const {
    return image_filename;
}

void ImageToScore2::setMaximumVoiceCount(size_t maximum_voice_count_) {
    maximum_voice_count = maximum_voice_count_;
}

size_t ImageToScore2::getMaximumVoiceCount() const {
    return maximum_voice_count;
}

void ImageToScore2::gaussianBlur(double sigma_x_, double sigma_y_, int kernel_size_, int kernel_shape_) {
    do_blur = true;
    sigma_x = sigma_x_;
    sigma_y = sigma_y_;
    kernel_size = kernel_size_;
    kernel_shape = kernel_shape_;
}

void ImageToScore2::condense(int row_count_) {
    do_condense = true;
    row_count = row_count_;
}

void ImageToScore2::contrast(double gain_, double bias_) {
    do_contrast = true;
    gain = gain_;
    bias = bias_;
}

void ImageToScore2::dilate(int kernel_shape_, int kernel_size_, int iterations_) {
    do_dilate = true;
    kernel_size = kernel_size_;
    kernel_shape = kernel_shape_;
    iterations = iterations_;
}

void ImageToScore2::erode(int kernel_shape_, int kernel_size_, int iterations_) {
    do_erode = true;
    kernel_size = kernel_size_;
    kernel_shape = kernel_shape_;
    iterations = iterations_;
}

void ImageToScore2::sharpen(int kernel_size_, double sigma_x_, double sigma_y_, double alpha_,
                            double beta_, double gamma_) {
    do_sharpen = true;
    sigma_x = sigma_x_;
    sigma_y = sigma_y_;
    alpha = alpha_;
    beta = beta_;
    gamma = gamma_;
}

void ImageToScore2::threshhold(double value_threshhold_) {
    do_threshhold = true;
    value_threshhold = value_threshhold_;
}

void ImageToScore2::write_processed_file(std::string operation, const cv::Mat &processed_image) const {
    char buffer[0x200];
    std::snprintf(buffer, 0x200, "%s-%s.jpg", image_filename.c_str(), operation.c_str());
    cv::imwrite(buffer, processed_image);
}

void ImageToScore2::processImage(){
  System::inform("BEGAN ImageToScore2::processImage()...\n");
  System::inform("image_filename: %s\n", image_filename.c_str());
  original_image = cv::imread(image_filename, cv::IMREAD_COLOR);
  System::inform("Read image: columns: %d rows: %d type: %d depth: %d\n", original_image.cols, original_image.rows, original_image.type(), original_image.depth());
  // First we process the image, then we translate it.
  cv::Mat source_image = original_image;
  if (do_blur) {
      System::inform("Blur...\n");
      cv::Mat output_image;
      cv::GaussianBlur (source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
      write_processed_file("Blur", output_image);
      source_image = output_image;
  }
  if (do_sharpen) {
      System::inform("Sharpen...\n");
      cv::Mat output_image;
      cv::GaussianBlur(source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
      cv::Mat sharpened_image;
      cv::addWeighted(source_image, alpha, output_image, beta, gamma, sharpened_image);
      write_processed_file("Sharpen", output_image);
      source_image = sharpened_image;
  }
  if (do_erode) {
      System::inform("Erode..\n");
      cv::Mat output_image;
      cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
      for (int i = 0; i < iterations; ++i) {
        cv::erode(source_image, output_image, kernel);
        source_image = output_image;
      }
      write_processed_file("Erode", output_image);
  }
  if (do_dilate) {
      System::inform("Dilate...\n");
      cv::Mat output_image;
      cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
      for (int i = 0; i < iterations; ++i) {
        cv::dilate(source_image, output_image, kernel);
        source_image = output_image;
      }
      write_processed_file("Dilate", output_image);
  }
  if (do_contrast) {
      System::inform("Contrast...\n");
      cv::Mat output_image =  cv::Mat::zeros( source_image.size(), source_image.type() );
      source_image.convertTo(output_image, -1, gain, bias);
      write_processed_file("Contrast", output_image);
      source_image = output_image;
  }
  if (do_threshhold) {
      System::inform("Threshhold...\n");
      cv::Mat output_image;
      cv::threshold(source_image, output_image, value_threshhold, 0, cv::THRESH_TOZERO);
      write_processed_file("Threshhold", output_image);
      source_image = output_image;
  }
  if (do_condense) {
      System::inform("Condense...\n");
      cv::Mat output_image;
      cv::resize(source_image, output_image, cv::Size(source_image.cols, row_count), cv::INTER_AREA);
      System::inform("New size columns: %5d  rows: %5d\n", output_image.cols, output_image.rows);
      write_processed_file("Condense", output_image);
      source_image = output_image;
  }
  processed_image = source_image;
  System::inform("Ended ImageToScore2::processImage().\n");
}

void ImageToScore2::pixel_to_event(int column, int row, const HSV &hsv, Event &event_) const {
    double status = 144.;
    event_.setStatus(status);
    double time_ = double(column) / processed_image.cols;
    time_ = time_ * score.scaleTargetRanges.getTime() + score.scaleTargetMinima.getTime();
    event_.setTime(time_);
    double instrument = double(hsv.h) / 255.;
    instrument = instrument * score.scaleTargetRanges.getInstrument() + score.scaleTargetMinima.getInstrument();
    instrument = std::round(instrument);
    event_.setInstrument(instrument);
    double key = double(processed_image.rows - row) / processed_image.rows;
    key = key * score.scaleTargetRanges.getKey() + score.scaleTargetMinima.getKey();
    key = std::round(key);
    event_.setKey(key);
    double velocity = double(hsv.v) / 255.;
    velocity = velocity * score.scaleTargetRanges.getVelocity() + score.scaleTargetMinima.getVelocity();
    event_.setVelocity(velocity);
}

void ImageToScore2::generate() {
    System::inform("BEGAN ImageToScore2::generate()...\n");
    // Processing the image before translating can reduce the number
    // of salient notes.
    processImage();
    // Translate a processed image to notes. The width dimension of the image
    // represents time, and the height dimension of the image represents pitch.
    // The basic idea is that, along each row of pixels, if the value goes above
    // a threshhold, a note is starting; if the value goes below that
    // threshhold, the note is ending. It might be that too many notes are
    // created at a particular time. Therefore, the notes are ordered by
    // salience and only the most salient are retained. First of course we
    // translate to HSV, which seems to be the best color model for our
    // purposes.
    //
    // In OpenCV, {0, 0} is the center of the upper left pixel.
    cv::cvtColor(processed_image, processed_image, cv::COLOR_BGR2HSV_FULL);
    Event startingEvent;
    Event endingEvent;
    Score startingEvents;
    Score endingEvents;
    HSV prior_pixel;
    HSV current_pixel;
    HSV next_pixel;
    auto note_less = [](const csound::Event &a, const csound::Event &b) {
        if (a.getChannel() < b.getChannel()) {
            return true;
        }
        if (a.getTime() < b.getTime()) {
            return true;
        }
        if (a.getOffTime() < b.getOffTime()) {
            return true;
        }
        if (a.getKeyNumber() < b.getKeyNumber()) {
            return true;
        }
        return false;
    };
    auto unique_notes = std::set<csound::Event, decltype(note_less)>(note_less);
    // No more than one note can start at the same time on the same row, even
    // though several adjacent rows can be mapped to the same MIDI key.
    std::map<int, csound::Event> pending_events;
    for (int column = 0; column < processed_image.cols; ++column) {
        System::debug("Processing column %d...\n", int(column));
        // Find starting events, i.e. events based on pixels whose value
        // exceeds the threshhold but the prior pixels did not.
        // The lowest note is row 0.
        for (int row = 0; row < processed_image.rows; ++row) {
            if (column == 0) {
                prior_pixel.clear();
            } else {
                prior_pixel = *processed_image.ptr<HSV>(row, column - 1);
            }
            current_pixel = *processed_image.ptr<HSV>(row, column);
            if (column < processed_image.cols) {
                next_pixel = *processed_image.ptr<HSV>(int(row), column + 1);
            } else {
                next_pixel.clear();
            }
            // A note is starting.
            // Another way of doing this is, if the value increases.
            if (prior_pixel.v <= value_threshhold && current_pixel.v > value_threshhold) {
                pixel_to_event(column, row, current_pixel, startingEvent);
                if (pending_events.find(row) == pending_events.end() && pending_events.size() < getMaximumVoiceCount()) {
                    if (System::getMessageLevel() >= System::DEBUGGING_LEVEL) {
                      System::debug("Starting event at   column: %5d row: %5d value: %5d  %s\n", column, row, current_pixel.v, startingEvent.toString().c_str());
                    }
                    pending_events[row] = startingEvent;
                }
            }
            // A note is ending.
            // Another way of doing this is, if the value decreases.
            if (current_pixel.v > value_threshhold && next_pixel.v <= value_threshhold) {
                if (pending_events.find(row) != pending_events.end()) {
                    csound::Event new_note = pending_events[row];
                    new_note.setOffTime(endingEvent.getOffTime());
                    if (System::getMessageLevel() >= System::DEBUGGING_LEVEL) {
                      System::debug("Ending event at     column: %5d row: %5d value: %5d  %s\n", column, row, current_pixel.v, new_note.toString().c_str());
                    }
                    pixel_to_event(column, row, current_pixel, endingEvent);
                    score.append(new_note);
                    pending_events.erase(row);
                }
            }
        }
        // TODO: Thin the score to remove less salient events, by tracking
        // overlapping notes and sorting them by salience and clering less
        // salient ones.
        score.sort();
    }
    System::inform("ENDED ImageToScore2::generate() with %d events.\n", score.size());
}

}
