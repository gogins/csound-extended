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
#include <FL/Fl_BMP_Image.H>
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
    } else if(imageFilename.find(".bmp") != std::string::npos || imageFilename.find(".BMP") != std::string::npos) {
        image = new Fl_BMP_Image(imageFilename.c_str());
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

void ImageToScore2::pixel_to_event(double x, double y, const HSV &hsv, Event &event) const {
    event[Event::STATUS] = 144;
    event[Event::TIME] = ((x / double(transformed_image.cols)) * score.scaleTargetRanges[Event::TIME]) + score.scaleTargetMinima[Event::TIME];
    event[Event::INSTRUMENT] = (hsv.h * score.scaleTargetRanges[Event::INSTRUMENT]) + score.scaleTargetMinima[Event::INSTRUMENT];
    event[Event::KEY] = int((((transformed_image.rows - y) / transformed_image.rows) * score.scaleTargetRanges[Event::KEY]) + score.scaleTargetMinima[Event::KEY] + 0.5);
    event[Event::VELOCITY] = (hsv.v * score.scaleTargetRanges[Event::VELOCITY]) + score.scaleTargetRanges[Event::VELOCITY];
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

void ImageToScore2::dilate(int kernel_shape_, int kernel_size_) {
    do_dilate = true;
    kernel_size = kernel_size_;
    kernel_shape = kernel_shape_;
}

void ImageToScore2::erode(int kernel_shape_, int kernel_size_) {
    do_erode = true;
    kernel_size = kernel_size_;
    kernel_shape = kernel_shape_;
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

void ImageToScore2::generate() {
    System::inform("BEGAN ImageToScore2::generate()...\n");
    System::inform("image_filename: %s\n", image_filename.c_str());
    original_image = cv::imread(image_filename, cv::IMREAD_COLOR);
    System::inform("Read image: columns: %d rows: %d type: %d depth: %d\n", original_image.cols, original_image.rows, original_image.type(), original_image.depth());
    if (show_steps) {
        cv::imshow("Original", original_image);
        cv::waitKey(0);
    }
    // First we process the image, then we translate it.
    cv::Mat source_image = original_image;
    if (do_blur) {
        cv::Mat output_image;
        cv::GaussianBlur (source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
        if (show_steps) {
            cv::imshow("Blur", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    if (do_sharpen) {
        cv::Mat output_image;
        cv::GaussianBlur(source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
        if (show_steps) {
            cv::imshow("Blur", output_image);
            cv::waitKey(0);
        }
        cv::Mat sharpened_image;
        cv::addWeighted(source_image, alpha, output_image, beta, gamma, sharpened_image);
        if (show_steps) {
            cv::imshow("Sharpen", sharpened_image);
            cv::waitKey(0);
        }
        source_image = sharpened_image;
    }
    if (do_erode) {
        cv::Mat output_image;
        cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
        cv::erode(source_image, output_image, kernel);
        if (show_steps) {
            cv::imshow("Erode", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    if (do_dilate) {
        cv::Mat output_image;
        cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
        cv::dilate(source_image, output_image, kernel);
        if (show_steps) {
            cv::imshow("Erode", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    if (do_contrast) {
        cv::Mat output_image =  cv::Mat::zeros( source_image.size(), source_image.type() );
        source_image.convertTo(output_image, -1, gain, bias);
        if (show_steps) {
            cv::imshow("Erode", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    if (do_threshhold) {
        cv::Mat output_image;
        cv::threshold(source_image, output_image, value_threshhold, 0, cv::THRESH_TOZERO);
        if (show_steps) {
            cv::imshow("Threshhold", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    if (do_condense) {
        cv::Mat output_image;
        cv::resize(source_image, output_image, cv::Size(source_image.cols, row_count));
        if (show_steps) {
            cv::imshow("Condense", output_image);
            cv::waitKey(0);
        }
        source_image = output_image;
    }
    transformed_image = source_image;
    // Now translate the processed image to notes. The width dimension of the
    // image represents time, and the height dimension of the image represents
    // pitch. The basic idea is that, along each row of pixels, if the value
    // goes above a threshhold, a note is starting; if the value goes below that
    // threshhold, the note is ending. It might be that too many notes are
    // created at a particular time. Therefore, the notes are ordered by
    // salience and only the most salient are retained. First of course we
    // translate to HSV, which seems to be the best color model for our
    // purposes. Processing the image before translating can reduce the number
    // of salient notes.
    cv::cvtColor(transformed_image, source_image, cv::COLOR_BGR2HSV_FULL);
    Event startingEvent;
    Event endingEvent;
    // Index is round(velocity * 1000 + channel).
    std::map<int, Event> startingEvents;
    // Index is round(key).
    std::map<int, Event> pendingEvents;
    HSV prior_pixel;
    HSV current_pixel;
    HSV next_pixel;
    for (double column = 0; column < source_image.cols; ++column) {
        // Find starting events, i.e. an event based on a pixel whose value
        // exceeds the threshhold but the prior pixel did not.
        for (double row = 0;
                row < source_image.rows;
                ++row) {
            if (column == 0.0) {
                prior_pixel.clear();
            } else {
                prior_pixel = *source_image.ptr<HSV>(int(row), int(column - 1));
            }
            current_pixel = *source_image.ptr<HSV>(int(row), int(column));
            if (prior_pixel.v <= value_threshhold && current_pixel.v > value_threshhold) {
                pixel_to_event(column, row, current_pixel, startingEvent);
                System::debug("Starting event at  (x =%5d, y =%5d, value =%5d): %s\n", size_t(column), size_t(row), current_pixel.v, startingEvent.toString().c_str());
                int startingEventsIndex = int(startingEvent.getVelocityNumber() * 1000.0 + startingEvent.getChannel());
                startingEvents[startingEventsIndex] = startingEvent;
            }
            // Insert starting events into the pending event list, in order of
            // decreasing loudness, until the pending event list has no more than
            // maximumCount events...
            for (std::map<int, Event>::reverse_iterator startingEventsIterator = startingEvents.rbegin();
                    startingEventsIterator != startingEvents.rend();
                    ++startingEventsIterator) {
                if (pendingEvents.size() < maximum_voice_count) {
                    int pendingEventIndex = startingEventsIterator->second.getKeyNumber();
                    // ...but do not interrupt an already playing event.
                    if (pendingEvents.find(pendingEventIndex) == pendingEvents.end()) {
                        System::debug("Pending event at   (x =%5d, y =%5d, value =%5d): %s\n", size_t(column), size_t(row), current_pixel.v, startingEventsIterator->second.toString().c_str());
                        pendingEvents[pendingEventIndex] = startingEventsIterator->second;
                    }
                } else {
                    break;
                }
            }
            // Remove ending events from the pending event list and insert them into
            // the score.
            for (double row = 0;
                    row < source_image.rows;
                    ++row) {
                if (column < source_image.cols) {
                    next_pixel = *source_image.ptr<HSV>(int(row), int(column + 1));
                } else {
                    next_pixel.clear();
                }
                current_pixel = *source_image.ptr<HSV>(int(row), int(column));
                if (current_pixel.v > value_threshhold && next_pixel.v <= value_threshhold) {
                    pixel_to_event(column, row, next_pixel, endingEvent);
                    System::debug("Ending event at    (x =%5d, y =%5d, value =%5d): %s\n", size_t(column), size_t(row), current_pixel.v, endingEvent.toString().c_str());
                    int pendingEventIndex = endingEvent.getKeyNumber();
                    if (pendingEvents.find(pendingEventIndex) != pendingEvents.end()) {
                        Event &startingEvent = pendingEvents[pendingEventIndex];
                        startingEvent.setDuration(endingEvent.getTime() - startingEvent.getTime());
                        if (startingEvent.getDuration() > 0.0) {
                            score.push_back(startingEvent);
                            System::inform("Inserting event at (x =%5d, y =%5d):                   %s\n", size_t(column), size_t(row), startingEvent.toString().c_str());
                            System::inform("Events pending=        %5d\n", pendingEvents.size());
                        }
                    }
                    pendingEvents.erase(pendingEventIndex);
                }
            }
        }
    }
    System::inform("ENDED ImageToScore2::generate() with %d events.\n", score.size());
}

}
