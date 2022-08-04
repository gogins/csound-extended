#ifndef IMAGETOSCORE_HPP_INCLUDED
#define IMAGETOSCORE_HPP_INCLUDED
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

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include "Silence.hpp"
%}
#else
#include "Silence.hpp"
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>
#endif

class Fl_Image;

namespace csound
{

/**
* Translates images files to scores.
* The OpenCV library is used to do an improved mapping
* from images to scores. Various image processing algorithms may be applied to
* the original image before the resulting image is translated to notes. Any
* number of such operations may be specified, but the order of processing is
* fixed.
*/
class SILENCE_PUBLIC ImageToScore2 : public ScoreNode
{
protected:
    std::string image_filename;
    cv::Mat original_image;
    cv::Mat processed_image;
    size_t maximum_voice_count = 7;
    virtual void pixel_to_event(int column, int row, const cv::Vec3f &hsv, Event &event) const;
public:
    ImageToScore2();
    virtual ~ImageToScore2();
    virtual void setImageFilename(std::string imageFilename);
    virtual std::string getImageFilename() const;
    virtual void setMaximumVoiceCount(size_t maximumVoiceCount);
    virtual size_t getMaximumVoiceCount() const;
    /**
     * Blur the image using a Gaussian kernel before translating the image to
     * notes. Kernel size should be odd.
     */
    virtual void gaussianBlur(double sigma_x_, double sigma_y_ = 0, int kernel_size_ = 9, int kernel_shape_ = cv::MORPH_RECT);
    bool do_blur = false;
    double sigma_x;
    double sigma_y;
    int kernel_shape = cv::MORPH_RECT;
    int kernel_size = 9;
    /**
     * Translate the image to the specified number of rows before translating
     * it to notes. If this operation is performed, it is always the last
     * operation before translating the resulting image to notes.
     */
    virtual void condense(int row_count_);
    bool do_condense = false;
    int row_count = -1;
    /**
     * Change the contrast of the image by the specified factors before
     * translating it to notes.
     */
    virtual void contrast(double gain_, double bias_);
    double gain = 0;
    double bias = 0;
    bool do_contrast = false;
    /**
     * Increase the thickness of features in the image before translating it to
     * notes.
     */
    virtual void dilate(int kernel_shape_, int kernel_size_, int iterations = 1);
    bool do_dilate = false;
    /**
     * Decrease the thickness of features in the image before translating it to
     * notes.
     */
    virtual void erode(int kernel_shape, int kernel_size_, int iterations = 1);
    bool do_erode = false;
    int iterations = 1;
    /**
     * Sharpen the image before translating to notes. First the image is
     * blurred, and then the blurred image is subtracted from the original
     * image.
     */
    virtual void sharpen(int kernel_size_, double sigma_x_, double sigma_y_, double alpha_,
        double beta_, double gamma_);
    bool do_sharpen = false;
    double alpha = 0;
    double beta = 0;
    double gamma = 0;
    /**
     * Set all values less than the threshhold to zero before translating the
     * image to notes.
     */
    virtual void threshhold(double value_threshhold_);
    bool do_threshhold = false;
    double value_threshhold = 0;
    void write_processed_file(std::string operation, const cv::Mat &processed_image) const;
    
    /**
     * Perform any image processing, then translate the resulting image to
     * notes.
     */
    virtual void processImage();
    virtual void generate();
};

/**
 * Only for backwards compatibility.
 */
typedef ImageToScore2 ImageToScore;

}

#endif