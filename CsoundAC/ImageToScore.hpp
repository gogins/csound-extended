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
#ifndef IMAGETOSCORE_H
#define IMAGETOSCORE_H

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include "Silence.hpp"
%}
#else
#include "Silence.hpp"
#include <opencv2/core.hpp>
#endif

class Fl_Image;

namespace csound
{
/**
* Translates images in various RGB formats to scores.
* Hue is mapped to instrument, value is mapped to loudness.
*/
class SILENCE_PUBLIC ImageToScore : public ScoreNode
{
protected:
    std::string imageFilename;
    Fl_Image *image;
    size_t maximumVoiceCount;
    double minimumValue;
    static void rgbToHsv(double r, double g, double b, double &h, double &s, double &v);
    virtual void getPixel(size_t x, size_t y, double &hue, double &saturation, double &value) const;
    virtual void translate(double x, double y, double hue, double value, Event &event) const;
public:
    ImageToScore(void);
    virtual ~ImageToScore(void);
    virtual void setImageFilename(std::string imageFilename);
    virtual std::string getImageFilename() const;
    virtual void setMaximumVoiceCount(size_t maximumVoiceCount);
    virtual size_t getMaximumVoiceCount() const;
    virtual void setMinimumValue(double minimumValue);
    virtual double getMinimumValue() const;
    virtual void generate();
};

/**
* Translates images files to scores.
* The OpenCV library is used to do an improved mapping
* from images to scores.
*/
class SILENCE_PUBLIC ImageToScore2 : public ScoreNode
{
protected:
    std::string image_filename;
    cv::Mat original_image;
    cv::Mat transformed_image;
    size_t maximum_voice_count;
    size_t instrument_count;
    size_t semitone_count;
    double value_threshhold;
    virtual void pixel_to_event(double x, double y, double hue, double value, Event &event) const;
public:
    ImageToScore2(void);
    virtual ~ImageToScore2(void);
    virtual void setImageFilename(std::string imageFilename);
    virtual std::string getImageFilename() const;
    virtual void setMaximumVoiceCount(size_t maximumVoiceCount);
    virtual size_t getMaximumVoiceCount() const;
    virtual void setMinimumValue(double value_threshhold);
    virtual double getMinimumValue() const;
    virtual void generate();
};

}

#endif
