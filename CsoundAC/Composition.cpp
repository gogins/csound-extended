/**
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
#if defined(_MSC_VER) && !defined(__GNUC__)
#pragma warning (disable:4786)
#endif
#include "Composition.hpp"
#include "System.hpp"
#include <algorithm>
#include <cstdlib>
#include <unistd.h>
#include <map>

namespace csound
{
Composition::Composition() :
    tonesPerOctave(12.0),
    conformPitches(false),
    tieOverlappingNotes(false),
    duration(0),
    score(baseScore)
{
}

Composition::~Composition()
{
}

void Composition::setScore(Score &score_) 
{
    score = score_;
}

int Composition::render()
{
    clear();
    int errorStatus = generate();
    ///timestamp = makeTimestamp();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = perform();
    return errorStatus;
}

int Composition::renderAll()
{
    clear();
    int errorStatus = generate();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = performAll();
    return errorStatus;
}

int Composition::perform()
{
    return 0;
}

int Composition::generate()
{
    return 0;
}

void Composition::clear()
{
    score.clear();
}

Score &Composition::getScore()
{
    return score;
}

void Composition::write(const char *text)
{
    System::message(text);
}

void Composition::setTonesPerOctave(double tonesPerOctave)
{
    this->tonesPerOctave = tonesPerOctave;
}

double Composition::getTonesPerOctave() const
{
    return tonesPerOctave;
}

void Composition::setConformPitches(bool conformPitches)
{
    this->conformPitches = conformPitches;
}

bool Composition::getConformPitches() const
{
    return conformPitches;
}

std::string Composition::getOutputDirectory() const
{
    if (output_directory.empty() == true) {
        char buffer[PATH_MAX];
        getcwd(buffer, PATH_MAX);
        return buffer;
    }
    return output_directory;
}

void Composition::setOutputDirectory(std::string output_directory)
{
    output_directory = output_directory;
}

std::string Composition::getFilename() const
{
    return stem;
}

void Composition::setFilename(std::string filename)
{
    this->stem = filename;
}

std::string Composition::getFileFilepath() const
{
    return base_filepath;
}

std::string Composition::getBasename() const
{
    return base_filepath;
}

std::string Composition::generateFilename()
{
    char buffer[0x200];
    snprintf(buffer, 0x200, "silence.%s.py", makeTimestamp().c_str());
    return buffer;
}

std::string Composition::getMidifileFilepath() const
{
    return midi_filepath;
}

std::string Composition::getOutputSoundfileFilepath() const
{
    if (output_filename.empty())
    {
        return master_filepath;
    }
    else
    {
        return output_filename;
    }
}

std::string Composition::getNormalizedSoundfileFilepath() const
{
    return normalized_master_filepath;
}

std::string Composition::getCdSoundfileFilepath() const
{
    return cd_quality_filepath;
}

std::string Composition::getMp3SoundfileFilepath() const
{
    return mp3_filepath;
}

std::string Composition::getMusicXmlfileFilepath() const
{
    std::string name = base_filepath;
    name.append(".xml");
    return name;
}

std::string Composition::getFomusfileFilepath() const
{
    std::string name = getBasename();
    name.append(".fms");
    return name;
}

std::string Composition::getLilypondfileFilepath() const
{
    std::string name = getBasename();
    name.append(".ly");
    return name;
}

std::string Composition::makeTimestamp()
{
    time_t time_ = 0;
    time(&time_);
    struct tm* tm_ = gmtime(&time_);
    char buffer[0x200];
    strftime(buffer, 0x200, "%Y-%m-%d.%H-%M-%S", tm_);
    return buffer;
}

std::string Composition::getTimestamp() const
{
    return timestamp;
}

int Composition::tagFile(std::string filename) const
{
    std::string command = "bwfmetaedit";
    command = command + " --OriginationDate=" + getTimestamp().substr(0, 10);
    command = command + " --ICRD=" + getTimestamp().substr(0, 10);
    if (getTitle().length() > 1) {
        command = command + " --Description=\"" + getTitle() + "\"";
        command = command + " --INAM=\"" + getTitle() + "\"";
    }
    if (getCopyright().length() > 1) {
        command = command + " --ICOP=\"" + getCopyright() + "\"";
    }
    if (getArtist().length() > 1) {
        command = command + " --Originator=\"" + getAuthor() + "\"";
        command = command + " --IART=\"" + getArtist() + "\"";
    }
    if (getAlbum().length() > 1) {
        command = command + " --IPRD=\"" + getAlbum() + "\"";
    }
    if (getLicense().length() > 1) {
        command = command + " --ICMT\"=" + getLicense() + "\"";
    }
    command = command + " " + filename.c_str();
    System::inform("tagFile(): %s\n", command.c_str());
    return std::system(command.c_str());
}

int Composition::performMaster()
{
    System::inform("BEGAN Composition::performMaster()...\n");
    int errorStatus = perform();
    System::inform("ENDED Composition::performMaster().\n");
    return errorStatus;
}

int Composition::performAll()
{
    System::inform("BEGAN Composition::performAll()...\n");
    auto started_at = System::startTiming();
    int errorStatus = performMaster();
    if (errorStatus) {
        return errorStatus;
    }
    double seconds = System::stopTiming(started_at);
    System::message("Performance took %9.4f seconds.\n", seconds);
    started_at = System::startTiming();
    errorStatus = translateMaster();
    System::inform("ENDED Composition::performAll().\n");
    return errorStatus;
}

int Composition::translateMaster()
{
    auto started_at = System::startTiming();
    System::inform("BEGAN Composition::translateMaster...\n");
    int errorStatus = tagFile(getOutputSoundfileFilepath());
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = normalizeOutputSoundfile();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = translateToCdAudio();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = translateToMp3();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = translateToMp4();
    auto seconds = System::stopTiming(started_at);
    System::message("Translation took %9.4f seconds.\n", seconds);
    System::inform("ENDED Composition::translateMaster.\n");
    return errorStatus;
}

int Composition::normalizeOutputSoundfile(double levelDb)
{
    char buffer[0x200];
    std::snprintf(buffer,
                  0x200,
                  "sox %s -V3 -b 32 -e floating-point %s gain -n %f\n",
                  getOutputSoundfileFilepath().c_str(),
                  getNormalizedSoundfileFilepath().c_str(),
                  levelDb);
    int errorStatus = std::system(buffer);
    if (errorStatus) {
        return errorStatus;
    }
    System::inform("Composition::normalizeOutputSoundfile(): %s", buffer);
    errorStatus = tagFile(getNormalizedSoundfileFilepath());
    return errorStatus;
}

int Composition::translateToCdAudio(double levelDb)
{
    char buffer[0x200];
    std::snprintf(buffer, 0x200, "sox %s -V3 -b 16 %s gain -n %f rate 44100\n",
                  getOutputSoundfileFilepath().c_str(),
                  getCdSoundfileFilepath().c_str(),
                  levelDb);
    System::inform("Composition::translateToCdAudio(): %s", buffer);
    int errorStatus = std::system(buffer);
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = tagFile(getCdSoundfileFilepath());
    return errorStatus;
}

int Composition::translateToMp3(double bitrate, double levelDb)
{
    char buffer[0x200];
    char copyright_[0x100];
    std::snprintf(copyright_, 0x100, "(C) %s by %s", year.c_str(), author.c_str());
    std::snprintf(buffer,
                  0x200,
                  "lame --verbose --disptime 2 --nohist --preset cd --tt \"%s\" --ta \"%s\" --tl \"%s\" --tc \"%s\" %s %s\n",
                  getTitle().c_str(),
                  getAuthor().c_str(),
                  getAlbum().c_str(),
                  getCopyright().c_str(),
                  getCdSoundfileFilepath().c_str(),
                  getMp3SoundfileFilepath().c_str());
    System::inform("Composition::translateToMp3(): %s", buffer);
    int errorStatus = std::system(buffer);
    return errorStatus;
}

int Composition::translateToMp4()
{
    char sox_spectrogram_command[0x200];
    std::snprintf(sox_spectrogram_command, 0x200, "sox -S %s -n spectrogram -o \"%s\" -t\"%s\" -c\"%s\"", cd_quality_filepath.c_str(), spectrogram_filepath.c_str(), getTitle().c_str(), getCopyright().c_str());
    System::inform("sox_spectrogram_command: %s\n", sox_spectrogram_command);
    int errorStatus = std::system(sox_spectrogram_command);
    char mp4_encode_command[0x500];
    std::sprintf(mp4_encode_command, "ffmpeg -r 1 -i \"%s\" -i \"%s\" -codec:a aac -strict -2 -b:a 384k -r:a 48000 -c:v libx264 -b:v 500k \"%s\"", spectrogram_filepath.c_str(), master_filepath.c_str(), mp4_filepath.c_str());
    System::inform("mp4_encode_command: %s\n", mp4_encode_command);
    errorStatus = std::system(mp4_encode_command);
    std::string mp4_metadata;
    mp4_metadata.append("-metadata title=\"" + stem + "\" ");
    mp4_metadata.append("-metadata album=\"" + album + "\" ");
    mp4_metadata.append("-metadata date=\"" + year + "\" ");
    mp4_metadata.append("-metadata track=\"" + track + "\" ");
    mp4_metadata.append("-metadata genre=\"" + notes + "\" ");
    mp4_metadata.append("-metadata publisher=\"" + performance_rights_organization + "\" ");
    mp4_metadata.append("-metadata copyright=\"" + copyright + "\" ");
    mp4_metadata.append("-metadata composer=\"" + author + "\" ");
    mp4_metadata.append("-metadata artist=\"" + artist + "\" ");
    char mp4_metadata_command[0x500];
    std::sprintf(mp4_metadata_command, "ffmpeg -y -loop 1 -framerate 2 -i \"%s\" -i \"%s\" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" %s \"%s\"", spectrogram_filepath.c_str(), master_filepath.c_str(), mp4_metadata.c_str(), mp4_filepath.c_str());
    System::inform("mp4_metadata_command: %s\n", mp4_metadata_command);
    errorStatus = std::system(mp4_metadata_command);
    return errorStatus;
}

/**
 * Generates all filenames and other text based on required stem,
 * output_directory, filename extension, and metadata.
 */
void Composition::generateAllNames()
{
    timestamp = makeTimestamp();
    output_directory = getOutputDirectory();
    char buffer[0x200];
    if (album.empty() == true || track.empty() == true) {
        std::snprintf(buffer, 0x200, "%s -- %s", author.c_str(), stem.c_str());
    } else {
        std::snprintf(buffer, 0x200, "%s -- %s -- Track %s -- %s", author.c_str(), album.c_str(), track.c_str(), stem.c_str());
    }
    label = buffer;
    // Avoid all spaces in all actual filenames.
    base_filepath = label;
    std::replace(base_filepath.begin(), base_filepath.end(), ' ', '_');
    base_filepath = output_directory + "/" + base_filepath;
    master_filepath = base_filepath + ".master.wav";
    normalized_master_filepath = base_filepath + ".norm.wav";
    spectrogram_filepath = base_filepath + ".png";
    cd_quality_filepath = base_filepath + ".cd.wav";
    mp3_filepath = base_filepath + ".mp3";
    mp4_filepath = base_filepath + ".mp4";
    flac_filepath = base_filepath + ".flac";
    midi_filepath = base_filepath+ + ".mid";
    if (notes.empty() == true) {
        notes = "Electroacoustic Music";
    }
    bext_description = notes;
    bext_originator = author;
    bext_orig_ref = master_filepath;
    //bext_umid              = xxx
    //bext_orig_date         = xxx
    //bext_orig_time         = xxx
    //bext_coding_hist       = xxx
    //bext_time_ref          = xxx
    if (performance_rights_organization.empty() == true) {
        std::snprintf(buffer, 0x200, "Copyright (C) %s by %s", year.c_str(), author.c_str());
    } else {
        std::snprintf(buffer, 0x200, "Copyright (C) %s by %s (%s)", year.c_str(), author.c_str(), performance_rights_organization.c_str());
    }
    copyright = buffer;
    if (artist.empty() == true) {
        artist = author;
    }
    album  = album;
    System::inform("timestamp:                       %s\n", timestamp.c_str());
    System::inform("author:                          %s\n", author.c_str());
    System::inform("title:                           %s\n", getTitle().c_str());
    System::inform("label:                           %s\n", label.c_str());
    System::inform("copyright:                       %s\n", copyright.c_str());
    System::inform("license:                         %s\n", getLicense().c_str());
    System::inform("output_directory:                %s\n", output_directory.c_str());
    System::inform("base_filepath:                   %s\n", base_filepath.c_str());
    System::inform("master_filepath:                 %s\n", master_filepath.c_str());
    System::inform("normalized_master_filepath:      %s\n", normalized_master_filepath.c_str());
    System::inform("cd_quality_filepath:             %s\n", cd_quality_filepath.c_str());
    System::inform("mp3_filepath:                    %s\n", mp3_filepath.c_str());
    System::inform("spectrogram_filepath:            %s\n", spectrogram_filepath.c_str());
    System::inform("mp4_filepath:                    %s\n", mp4_filepath.c_str());
    System::inform("bext_description:                %s\n", bext_description.c_str());
    System::inform("bext_originator:                 %s\n", bext_originator.c_str());
    System::inform("bext_orig_ref:                   %s\n", bext_orig_ref.c_str());
    System::inform("performance_rights_organization: %s\n", performance_rights_organization.c_str());
}

std::string Composition::getArtist() const
{
    return artist;
}

void Composition::setArtist(std::string value)
{
    artist = value;
}

std::string Composition::getTitle() const
{
    return stem;
}

void Composition::setTitle(std::string value)
{
    stem = value;
}

std::string Composition::getAuthor() const
{
    return author;
}

void Composition::setAuthor(std::string value)
{
    author = value;
}

std::string Composition::getYear() const
{
    return year;
}

void Composition::setYear(std::string value)
{
    year = value;
}

std::string Composition::getCopyright() const
{
    return copyright;
}

void Composition::setCopyright(std::string value)
{
    copyright = value;
}

std::string Composition::getPerformanceRightsOrganization() const
{
    return performance_rights_organization;
}

void Composition::setPerformanceRightsOrganization(std::string value)
{
    performance_rights_organization = value;
}

std::string Composition::getAlbum() const
{
    return album;
}

void Composition::setAlbum(std::string value)
{
    album = value;
}

std::string Composition::getLicense() const
{
    if (license.empty() == true) {
        return "CC BY-NC-ND 4.0";
    }
    return license;
}

void Composition::setLicense(std::string value)
{
    license = value;
}

int Composition::translateToNotation(const std::vector<std::string> partNames, std::string header)
{
    std::string filename = getFomusfileFilepath();
    std::ofstream stream;
    stream.open(filename.c_str(), std::ifstream::binary);
    char buffer[0x200];
    std::sprintf(buffer, "title = %s\n", getTitle().c_str());
    stream << buffer;
    if (getArtist().length() > 1) {
        std::sprintf(buffer, "author = %s\n", getArtist().c_str());
        stream << buffer;
    }
    stream << "beat = 1/64" << std::endl;
    stream << "timesig (4 4)" << std::endl;
    stream << "lily-papersize = 11x17" << std::endl;
    if (header.size() > 1) {
        stream << header.c_str();
    }
    if (partNames.size() > 0) {
        for (size_t partI = 0, partN = partNames.size(); partI < partN; ++partI) {
            std::sprintf(buffer, "part <id = %zu name = %s>\n", partI, partNames[partI].c_str());
            stream << buffer;
        }
    } else {
        for (size_t partI = 0, partN = 100; partI < partN; ++partI) {
            std::sprintf(buffer, "part <id = %zu name = Part%zu>\n", partI, partI);
            stream << buffer;
        }
    }
    std::map<int, std::vector<Event> > eventsForParts;
    for (size_t eventI = 0, eventN = score.size(); eventI < eventN; ++eventI) {
        const Event &event = score[eventI];
        if (event.isNoteOn()) {
            double duration = event.getDuration() * 32.0;
            duration = Conversions::round(duration);
            if (duration > 0) {
                int part = int(event.getInstrument() + 1);
                eventsForParts[part].push_back(event);
            }
        }
    }
    for (std::map<int, std::vector<Event> >::iterator it = eventsForParts.begin(); it != eventsForParts.end(); ++it) {
        int part = it->first;
        std::vector<Event> &events = it->second;
        for (std::vector<Event>::iterator eventI = events.begin(); eventI != events.end(); ++eventI) {
            Event &event = *eventI;
            if (eventI == events.begin()) {
                std::sprintf(buffer, "part %d\n", part);
            } else {
                double duration = event.getDuration() * 32.0;
                duration = Conversions::round(duration);
                std::sprintf(buffer, "time %g dur %g pitch %g;\n", event.getTime() * 32.0, duration, event.getKey());
            }
            stream << buffer;
        }
    }
    stream.close();
    std::sprintf(buffer, "fomus --verbose -i %s -o %s.xml", getFomusfileFilepath().c_str(), getTitle().c_str());
    int errorStatus = std::system(buffer);
    return errorStatus;
}

int Composition::processArgv(int argc, const char **argv)
{
    std::vector<std::string> args;
    for (int i = 0; i < argc; ++i)
    {
        args.push_back(argv[i]);
    }
    return processArgs(args);
}

int Composition::processArgs(const std::vector<std::string> &args)
{
    generateAllNames();
    return renderAll();
}

void Composition::setOutputSoundfileName(std::string name)
{
    output_filename = name;
}

void Composition::clearOutputSoundfileName()
{
    output_filename.clear();
}

void Composition::setTieOverlappingNotes(bool tieOverlappingNotes_) {
    tieOverlappingNotes = tieOverlappingNotes_;
}

bool Composition::getTieOverlappingNotes() const {
    return tieOverlappingNotes;
}

void Composition::setDuration(double seconds) {
    duration = seconds;
}

double Composition::getDuration() const {
    return duration;
}


}
