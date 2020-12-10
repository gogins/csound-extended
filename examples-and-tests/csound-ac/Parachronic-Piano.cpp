#include <Composition.hpp>
#include <eigen3/Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

#define PI      (3.141592653589793238462643383279502884197)
#define TWO_PI  (6.283185307179586476925286766559005768394)


/**
 * Generators are lambdas with the same signature as the following example.
 * The pen is the current position of a "pen" in the score. The pen is
 * "written" by appending it to the score. Additional events can be computed
 * based on the parameters of the function, and also appended to the score.
 * The function can, and usually does, move the pen to a new position before
 * returning it.
 */
auto generator = [&](const csound::Event &pen, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode)
{
    csound::Event result = pen;
    return result;
};

/**
 * Computes a deterministic, finite recurrent iterated function system by
 * recursively applying a set of generators (transformations) to a pen
 * which represents the position of a "pen" on a "score." The entries in
 * the transitions matrix represent open or closed paths of recurrence through
 * the tree of calls. Because the pen is passed by value, it is in effect
 * copied at each call of a generator and at each layer of recursion. The
 * generators may or may not append a local copy of the pen to the score,
 * thus "writing" a "note" or other event on the "score."
 */
void recurrent(std::vector< std::function<csound::Event(const csound::Event &,int, csound::Score &, csound::VoiceleadingNode &)> > &generators,
        Eigen::MatrixXd &transitions,
        int depth,
        int transformationIndex,
        const csound::Event pen,
        csound::Score &score,
        csound::VoiceleadingNode &voiceleadingNode)
{
    depth = depth - 1;
    if (depth == 0) {
        return;
    }
    // std::printf("recurrent(depth: %3d  index: %3d  %s)\n", depth, transformationIndex, pen.toString().c_str());
    // Index is that of the current transformation. The column vector at that index determines which
    // transformations may be applied.
    for (int transitionIndex = 0, transitionN = transitions.rows(); transitionIndex < transitionN; ++transitionIndex) {
        if (transitions(transformationIndex, transitionIndex)) {
            auto newCursor = generators[transitionIndex](pen, depth, score, voiceleadingNode);
            recurrent(generators, transitions, depth, transitionIndex, newCursor, score, voiceleadingNode);
        }
    }
}

/**
 * All composition and synthesis code is defined in the main function.
 * There is no need for any of this code to be in a separate file.
 */
int main(int argc, const char **argv)
{
    csound::MusicModel model;
    // These fields determine output filenames and ID 3 tags.
    model.setAuthor("Michael Gogins");
    model.setTitle("Parachronic-Piano");
    model.setAlbum("Silence");
    model.setYear("2018");
    model.setPerformanceRightsOrganization("Irreducible Productions, ASCAP");

    std::vector<std::function<csound::Event(const csound::Event &, int, csound::Score &, csound::VoiceleadingNode &)>> generators;

    auto g1 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode) {
        csound::Event pen = pen_;
        if (depth == 1) {
            score.append(pen);
        }
        if (depth <= 1) {
            return pen;
        }
        pen[csound::Event::TIME] =          (pen[csound::Event::TIME] *   0.7600) +  0.2125;
        pen[csound::Event::KEY] =           (pen[csound::Event::KEY]  *   1.0000) +  3.21050;
        pen[csound::Event::INSTRUMENT] =    4.0 * 1.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =      1.0;
        pen[csound::Event::PAN] =           .875;
        if (depth == 4) {
            //voiceleadingNode.C(pen[csound::Event::TIME], "Dm9");
            voiceleadingNode.Q(pen[csound::Event::TIME], -7);
        }
        if (depth == 3) {
            voiceleadingNode.K(pen[csound::Event::TIME]);
        }
        return pen;
    };
    generators.push_back(g1);

    auto g2 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode) {
        csound::Event pen = pen_;
        if (depth == 1) {
            score.append(pen);
        }
        if (depth <= 2) {
            return pen;
        }
        pen[csound::Event::TIME] =          (pen[csound::Event::TIME] *   0.8500) -  0.2700;
        pen[csound::Event::KEY] =           (pen[csound::Event::KEY]  *   0.5000) +  3.0000;
        pen[csound::Event::INSTRUMENT] =    4.0 * 2.0 + double(depth % 4);      
        pen[csound::Event::VELOCITY] =      2.0;                        
        pen[csound::Event::PAN] =           .675;
        return pen;
    };
    generators.push_back(g2);

    auto g3 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode) {
        csound::Event pen = pen_;
        if (depth == 1) {
            score.append(pen);
        }
        if (depth <= 1) {
            return pen;
        }
        ///pen[csound::Event::TIME] =       pen[csound::Event::TIME] * .65 - .27;
        pen[csound::Event::TIME] =          (pen[csound::Event::TIME] *   0.7500) -  0.2700;
        pen[csound::Event::KEY] =           (pen[csound::Event::KEY]  *   0.5000) -  3.0700;
        pen[csound::Event::INSTRUMENT] =    4.0 * 3.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =      1.0;
        pen[csound::Event::PAN] =           -.675;
        return pen;
    };
    generators.push_back(g3);

    auto g4 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode) {
        csound::Event pen = pen_;
        if (depth == 1) {
            score.append(pen);
        }
        if (depth <= 0) {
            return pen;
        }
        pen[csound::Event::TIME] =          (pen[csound::Event::TIME] *   0.7900) +  0.2800;
        pen[csound::Event::KEY] =           (pen[csound::Event::KEY]  *   0.5100) -  4.2500;
        pen[csound::Event::INSTRUMENT] =    4.0 * 4.5 + double(depth % 4);
        pen[csound::Event::VELOCITY] =      2.0;
        pen[csound::Event::PAN] =           -.875;
        if (depth == 2) {
            voiceleadingNode.Q(pen[csound::Event::TIME], -5);
        }
        if (depth == 3) {
            voiceleadingNode.K(pen[csound::Event::TIME]);
        }
        return pen;
    };
    generators.push_back(g4);

    auto g5 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleadingNode) {
        csound::Event pen = pen_;
        if (depth == 1) {
            score.append(pen);
        }
        if (depth <= 0) {
            return pen;
        }
        pen[csound::Event::TIME] =          (pen[csound::Event::TIME] *   0.7900) -  0.2900;
        pen[csound::Event::KEY] =           (pen[csound::Event::KEY]  *   0.5000) +  4.0000;
        pen[csound::Event::INSTRUMENT] =    4.0 * 3.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =      2.0;
        pen[csound::Event::PAN] =           -.875;
        return pen;
    };
    generators.push_back(g5);

    // Generate the score.
    csound::Event pen = {1,1,144,0,1,1,0,0,0,0,1};
    pen[csound::Event::DURATION] = 0.025;
    Eigen::MatrixXd transitions(generators.size(), generators.size());
    transitions <<  1, 1, 0, 1, 1,
                    1, 1, 0, 1, 1,
                    0, 1, 1, 0, 1,
                    1, 0, 1, 1, 1,
                    0, 1, 1, 0, 1;
    csound::ScoreNode scoreNode;
    csound::VoiceleadingNode voiceleadingNode;
    voiceleadingNode.setModality({0., 2., 5., 7., 11.});
    voiceleadingNode.rescaleTimes = true;
    voiceleadingNode.addChild(&scoreNode);
    model.addChild(&voiceleadingNode);
    csound::Score &score = scoreNode.getScore();
    recurrent(generators, transitions, 9, 0, pen, score, voiceleadingNode);
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, true,   0.0);
    score.rescale(csound::Event::KEY,           true, 36.0, true,  60.0);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,   0.0);
    score.rescale(csound::Event::DURATION,      true,  .30, true,   1.0);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.setDuration(240.0);
    std::cout << "set duration:           " << score.getDuration() << std::endl;
    //score.rescale(csound::Event::DURATION,      true,  0.25, true,   0.5);
    score.temper(12.);
    //score.tieOverlappingNotes(true);
    score.findScale();
    score.setDuration(60. * 10.);
    std::mt19937 mersenneTwister;
    std::uniform_real_distribution<> randomvariable(-0.9, +0.9);
    for (int i = 0, n = score.size(); i < n; ++i) {
         score[i].setPan(randomvariable(mersenneTwister));
    }
    for (int i = 0, n = score.size(); i < n; ++i) {
        auto original = score[i];
        csound::Event clone(original);
        clone.setTime(original.getTime() + std::sin(original.getTime() / 200.) * 2.);
        score.append(clone);
    }
    score.tieOverlappingNotes(true);
    score.sort();
    std::cout << "Final duration:         " << score.getDuration() << std::endl;

    model.setCsoundOrchestra(R"(
sr     = 44100
ksmps  = 20
nchnls = 2 ; Changed for WebAssembly output from: = 2
    
                alwayson        "PianoOut"
    
                ; Load the Pianoteq into memory.
gipianoteq      vstinit         "/home/mkg/pianoteq_linux_v630/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
                
                ; Print information about the Pianoteq, such as parameter names and numbers.
                vstinfo         gipianoteq
                
                ; Open the Pianoteq's GUI.
                ;vstedit         gipianoteq

                ; Send notes from the score to the Pianoteq.
                instr 1 
                ; MIDI channels are numbered starting at 0.
                ; p3 always contains the duration of the note.
                ; p4 contains the MIDI key number (pitch),
                ; p5 contains the MIDI velocity number (loudness),
imidichannel    init            0
                vstnote         gipianoteq, imidichannel, p4, p5, p3
                endin

                ; Send parameter changes to the Pianoteq.
                instr 2 
                ; p4 is the parameter number.
                ; p5 is the parameter value.
                vstparamset     gipianoteq, p4, p5 
                endin

                ; Send audio from the Pianoteq to the output.
                instr PianoOut 
ablankinput     init            0
aleft, aright   vstaudio        gipianoteq, ablankinput, ablankinput
                outs            aleft, aright
                endin
                                
            )");
    model.processArgv(argc, argv);
}

