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
#include <cmath>
#include <complex>
#include <set>
#include <functional>
#include <opencv2/imgcodecs.hpp>

namespace csound {

    ImageToScore2::ImageToScore2() {
    }

    ImageToScore2::~ImageToScore2() {
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

    void ImageToScore2::processImage() {
        System::inform("ImageToScore2::processImage...\n");
        System::inform("image_filename: %s\n", image_filename.c_str());
         auto loaded_image = cv::imread(image_filename);
        if(loaded_image.empty() == true) {
            System::error("Failed to load image from: \"%s\"\n", image_filename.c_str());
            return;
        }
        // Images are always converted to floats before working with them.
        // Channels are ordered Blue, Green, Red not Red, Green, Blue.
        loaded_image.convertTo(original_image, CV_32FC3);
         // Normalize channels that were 8 bits.
        if(original_image.elemSize1() == 1) {
            original_image *= 1./255.;
        }
        // Convert the image to the Hue, Saturation, Value color space.
        cv::cvtColor(original_image, original_image, cv::COLOR_BGR2HSV);
        System::inform("Loaded image file \"%s\".\n", image_filename.c_str());
        System::inform("Read image: columns: %d rows: %d type: %d depth: %d\n", original_image.cols, original_image.rows, original_image.type(), original_image.depth());
        // First we process the image, then we translate it.
        cv::Mat source_image = original_image;
        if(do_blur) {
            System::inform("Blur...\n");
            cv::Mat output_image;
            cv::GaussianBlur(source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
            write_processed_file("Blur", output_image);
            source_image = output_image;
        }
        if(do_sharpen) {
            System::inform("Sharpen...\n");
            cv::Mat output_image;
            cv::GaussianBlur(source_image, output_image, cv::Size(kernel_size, kernel_size), sigma_x, sigma_y);
            cv::Mat sharpened_image;
            cv::addWeighted(source_image, alpha, output_image, beta, gamma, sharpened_image);
            write_processed_file("Sharpen", output_image);
            source_image = sharpened_image;
        }
        if(do_erode) {
            System::inform("Erode..\n");
            cv::Mat output_image;
            cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
            for(int i = 0; i < iterations; ++i) {
                cv::erode(source_image, output_image, kernel);
                source_image = output_image;
            }
            write_processed_file("Erode", output_image);
        }
        if(do_dilate) {
            System::inform("Dilate...\n");
            cv::Mat output_image;
            cv::Mat kernel = cv::getStructuringElement(kernel_shape, cv::Size(kernel_size, kernel_size));
            for(int i = 0; i < iterations; ++i) {
                cv::dilate(source_image, output_image, kernel);
                source_image = output_image;
            }
            write_processed_file("Dilate", output_image);
        }
        if(do_contrast) {
            System::inform("Contrast...\n");
            cv::Mat output_image =  cv::Mat::zeros(source_image.size(), source_image.type());
            source_image.convertTo(output_image, -1, gain, bias);
            write_processed_file("Contrast", output_image);
            source_image = output_image;
        }
        if(do_threshhold) {
            System::inform("Threshhold...\n");
            cv::Mat output_image;
            cv::threshold(source_image, output_image, value_threshhold, 0, cv::THRESH_TOZERO);
            write_processed_file("Threshhold", output_image);
            source_image = output_image;
        }
        if(do_condense) {
            System::inform("Condense...\n");
            cv::Mat output_image;
            cv::resize(source_image, output_image, cv::Size(source_image.cols, row_count), cv::INTER_AREA);
            System::inform("New size columns: %5d  rows: %5d\n", output_image.cols, output_image.rows);
            write_processed_file("Condense", output_image);
            source_image = output_image;
        }
        processed_image = source_image;
        System::inform("ImageToScore2::processImage.\n");
    }

    void ImageToScore2::pixel_to_event(int column, int row, const cv::Vec3f &hsv, Event &event_) const {
        double status = 144.;
        event_.setStatus(status);
        double time_ = double(column) / processed_image.cols;
        time_ = time_ * score.scaleTargetRanges.getTime() + score.scaleTargetMinima.getTime();
        event_.setTime(time_);
        double instrument = hsv[0];
        instrument = instrument * score.scaleTargetRanges.getInstrument() + score.scaleTargetMinima.getInstrument();
        instrument = std::round(instrument);
        event_.setInstrument(instrument);
        double key = double(processed_image.rows - row) / processed_image.rows;
        key = key * score.scaleTargetRanges.getKey() + score.scaleTargetMinima.getKey();
        key = std::round(key);
        event_.setKey(key);
        double velocity = hsv[2];
        velocity = velocity * score.scaleTargetRanges.getVelocity() + score.scaleTargetMinima.getVelocity();
        event_.setVelocity(velocity);
    }

    void ImageToScore2::generate() {
        System::inform("ImageToScore2::generate...\n");
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
        cv::Vec3f prior_pixel;
        cv::Vec3f current_pixel;
        cv::Vec3f next_pixel;
        auto note_less = [](const csound::Event &a, const csound::Event &b) {
            if(a.getChannel() < b.getChannel()) {
                return true;
            }
            if(a.getTime() < b.getTime()) {
                return true;
            }
            if(a.getOffTime() < b.getOffTime()) {
                return true;
            }
            if(a.getKeyNumber() < b.getKeyNumber()) {
                return true;
            }
            return false;
        };
        auto unique_notes = std::set<csound::Event, decltype(note_less)>(note_less);
        // No more than one note can start at the same time on the same row, even
        // though several adjacent rows can be mapped to the same MIDI key.
        std::map<int, csound::Event> pending_events;
        for(int column = 0; column < processed_image.cols; ++column) {
            System::debug("Processing column %d...\n", int(column));
            // Find starting events, i.e. events based on pixels whose value
            // exceeds the threshhold but the prior pixels did not.
            // The lowest note is row 0.
            for(int row = 0; row < processed_image.rows; ++row) {
                if(column == 0) {
                    prior_pixel = 0.;
                } else {
                    prior_pixel = processed_image.at<cv::Vec3f>(row, column - 1);
                }
                current_pixel = processed_image.at<cv::Vec3f>(row, column);
                if(column < processed_image.cols) {
                    next_pixel = processed_image.at<cv::Vec3f>(int(row), column + 1);
                } else {
                    next_pixel = 0.;
                }
                // A note is starting.
                // Another way of doing this is, if the value increases.
                if(prior_pixel[2] <= value_threshhold && current_pixel[2] > value_threshhold) {
                    pixel_to_event(column, row, current_pixel, startingEvent);
                    if(pending_events.find(row) == pending_events.end() && pending_events.size() < getMaximumVoiceCount()) {
                        if(System::getMessageLevel() >= System::DEBUGGING_LEVEL) {
                            System::debug("Starting event at   column: %5d row: %5d value: %5d  %s\n", column, row, current_pixel[2], startingEvent.toString().c_str());
                        }
                        pending_events[row] = startingEvent;
                    }
                }
                // A note is ending.
                // Another way of doing this is, if the value decreases.
                if(current_pixel[2] > value_threshhold && next_pixel[2] <= value_threshhold) {
                    if(pending_events.find(row) != pending_events.end()) {
                        csound::Event new_note = pending_events[row];
                        new_note.setOffTime(endingEvent.getOffTime());
                        if(System::getMessageLevel() >= System::DEBUGGING_LEVEL) {
                            System::debug("Ending event at     column: %5d row: %5d value: %5d  %s\n", column, row, current_pixel[2], new_note.toString().c_str());
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
        System::inform("ImageToScore2::generate with %d events.\n", score.size());
    }

}
