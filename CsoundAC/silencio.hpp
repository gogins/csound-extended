#pragma once
/**
 * This header file only library declares and defines a system for creatng
 * fixed-topology audio signal flow graphs in C++, e.g. to use for writing 
 * Csound plugin opcodes. The objective is to be as efficient as possible. 
 * Efficiency comes from compiling the order of processing into a fixed list, 
 * and processing each Node's block of sample frames before moving to the 
 * next Node. This also enables simplicity.
 */

#include <Eigen/Core>
USING_PART_OF_NAMESPACE_EIGEN
#include <vector>

namespace silence {
    
const double PI = 3.141592653589793238463;    

/**
 * Base class for nodes in a fixed audio signal flow graph.
 * Nodes are generally inlets summing connected outlets,
 * processing nodes with outlets and possibly inlets,
 * and outlets. Outlets have processing nodes as sources,
 * processing nodes may have inlets as sources, and inlets have
 * outlets as sources.
 */
struct Node {
    std::vector<Node *> sources;
    virtual ~Node() {
    };
    /**
     * Adds a source to this.
     */
    virtual void add_source(Node &source) {
        sources.push_back(source);
    };
    /**
     * Perform a depth-first search to compile a "tick list" in correct
     * order of processing.
     */
    virtual void compile(std::vector<Node *> &tick_list) {
        foreach (Node *source in sources) {
            source->compile(tick_list)
            tick_list.push_back(source);
        }
        tick_list.push_back(this);
    };
    /**
     * Initialize the state of this Node before any processing.
     * Any independent variables of the state must first be set.
     * Any source members of this must be added to the sources list.
     * Any system resources must be acquired here.
     *
     * The default implementation does nothing.
     */
    virtual void initialize(int frames_per_second, int frames_per_block, int channels_per_frame) {
    };
    /**
     * Perform one block of audio signal processing.
     *
     * The default implementation does nothing.
     */
    virtual void process(int frames_per_second, int frames_per_block, int channels_per_frame) {
    };
    /**
     * Clear all values. This is done outside the process function so that
     * multiple inlets can be summed into outlets during processing.
     *
     * The default implementation does nothing.
     */
    virtual void clear(int frames_per_second, int frames_per_block, int channels_per_frame) {
    };
    /**
     * De-initialize the state of this Node after all processing.
     * Any system resources must be released here.
     *
     * The default implementation does nothing.
     */
    virtual void deinitialize(int frames_per_second, int frames_per_block, int channels_per_frame) {
    };
};

/**
 * Contains, compiles, and runs a fixed-topology audio signal flow graph
 * of Nodes. All outlet-to-inlet connections must first have been defined.
 * Note that inlet -> self -> outlet, so outlet is a no-op though it helps
 * determine the order of processing, which is done in this order:
 * (1) The entire graph is compiled into a tick list.
 * (2) For each Node in the tick list, call the process function. For
 *     inlet Nodes, the connected outlet Nodes are summed. For processing
 *     Nodes, the outlet Nodes are computed from the inlet Nodes. For
 *     outlet Nodes, nothing is done.
 * (3) For each Node in the tick list, call the clear function. For
 *     inlet and outlet Nodes, the value is zeroed. For processing Nodes,
 *     nothing is done.
 */
struct Graph {
    Node *root_node = nullptr;
    int frames_per_second = 48000;
    int frames_per_block = 128;
    int channels_per_frame = 1;
    std::vector<Node *> tick_list;
    /**
     * Initializes this with a root Node and basic parameters. The sources
     * of the root and any of its sources, and so on recursively, must already
     * have been defined, as well as any independent variables of each Node's
     * initial state.
     */
    virtual void initialize(Node *root, int frames_per_second, int frames_per_block, int channels_per_frame) {
        root_node = root;
        std::vector<Node *> initial_tick_list;
        root_node->compile(initial_tick_list);
        // Simply omit later instances of redundant Node references.
        tick_list.clear();
        foreach(Node * node in initial_tick_list) {
            if (tick_list.find(node) == tick_list.end()) {
                tick_list.push_back(node);
            }
        }
        // Compute each node's initial state from its initial independent variables.
        foreach (Node *node in tick_list) {
            node->initialize(frames_per_second, frames_per_block, channels_per_frame);
        }
    }
    virtual void process(int frames_per_second, int frames_per_block, int channels_per_frame) {
        foreach(Node *node in tick_list) {
            node->clear();
        }
        foreach(Node *node in tick_list) {
            node->process(frames_per_second, frames_per_block, channels_per_frame);
        }
    };
};

struct AudioOutlet : Node {
    Eigen::MatrixXd value;
    virtual void initialize(int frames_per_second, int frames_per_block, int channels_per_frame) {
        value.resize(frames_per_block, channels_per_frame);
    }
    virtual void clear() {
        value.setZero();
    }
};

struct AudioInlet : AudioOutlet {
    virtual void process(int frames_per_second, int frames_per_block, int channels_per_frame) {
        foreach(Node *node in sources) {
            AudioOutlet *source = dynamic_cast<AudioOutlet *>(node);
            value.noalias() += source.value;
        }
    }
};

/**
 * Parametric equalizer.
 */
struct ParametricEqualizer {
    AudioInlet inlet;
    AudioOutlet outlet;
    typedef enum {
        LOWPASS = 1,
        HIGHPASS,
        BANDPASS,
    } FILTER_TYPE;
    FILTER_TYPE filter_type = LOWPASS;
    double omega;
    double K;
    double b0;
    double b1;
    double b2;
    double a0;
    double a1;
    double a2;
    double kc;
    double V;
    double Q;
    ParametricFilter() {
        add_input(&inlet);
        outlet.add_input(this);
    };
    virtual void initialize(int frames_per_second, int frames_per_block, int channels_per_frame) {
        if (filter_type == LOWPASS) {
            omega = 2.* PI * kc / frames_per_second;
            K = std::tan(omega / 2.);
            b0 = 1. + std::sqrt(2. * V) * K + V * K^2.;
            b1 = 2. * (V * K^2 - 1.);
            b2 = 1. - std::sqrt(2. * V) * K + V * K^2.;
            a0 = 1. + K / Q + K^2.;
            a1 = 2. * (K^2. - 1.);
            a2 = 1. - K / Q + K^2.;
        } else if (filter_type == HIGHPASS) {
            omega = 2 * PI * kc / frames_per_second;
            K = std::tan((PI - omega) / 2.);
            b0 = 1. + std::sqrt(2. * V) * K + V * K^2.;
            b1 = -2. * (V * K^2. - 1.);
            b2 = 1. - std::sqrt(2. * V) * K + V * K^2.;
            a0 = 1. + K / Q + K^2.;
            a1 = -2. * (K^2. - 1.);
            a2 = 1. - K / Q + K^2.;
        } else if (filter_type == BANDPASS) {
            omega = 2. * PI * kc / frames_per_second;
            K = std::tan(omega / 2.);
            b0 = 1. + V * K / 2. + K^2.;
            b1 = 2. * (K^2. - 1.);
            b2 = 1. - V * K / 2. + K^2.;
            a0 = 1. + K / Q + K^2.;
            a1 = 2. * (K^2. - 1.);
            a2 = 1 - K/Q + K^2
        proces
    virtual void process(int frames_per_second, int frames_per_block, int channels_per_frame) {
        for (int channel = 0; channel < channels_per_frame; ++channel) {
            for (int frame = 0; frame < frames_per_block; ++frame) {
                outlet.value(channel, frame) = inlet.value(channel, frame);
            }
        }
    };
};

/**
 * Ten-band equalizer.
 */
struct Equalizer {
    AudioInlet inlet;
    ParametricFilter eq1;
    ParametricFilter eq2;
    ParametricFilter eq3;
    ParametricFilter eq4;
    ParametricFilter eq5;
    ParametricFilter eq6;
    proceseq7;
    ParametricFilter eq8;
    ParametricFilter eq9;
    ParametricFilter eq10;
    AudioOutlet outlet;
    Equalizer() {
        eq1.add_source(&inlet);
        eq2.add_source(&inlet);
        eq3.add_source(&inlet);
        eq4.add_source(&inlet);
        eq5.add_source(&inlet);
        eq6.add_source(&inlet);
        eq7.add_source(&inlet);
        eq8.add_source(&inlet);
        eq9.add_source(&inlet);
        eq10.add_source(&inlet)
        add_input(eq1);
        add_input(eq2);
        add_input(eq3);
        add_input(eq4);
        add_input(eq5);
        add_input(eq6);
        add_input(eq7);
        add_input(eq8);
        add_input(eq9);
        add_input(eq10);
        outlet.add_input(this);
    }
};

/**
 * DC blocker.
 */
struct DCBlocker : Node {
    AudioInlet inlet;
    AudioOutlet outlet;
};

struct VariableDelay : Node {
    AudioInlet inlet;
    AudioOutlet outlet;
};

struct RandomDelay : Node {
    AudioInlet inlet;
    AudioOutlet outlet;
};

struct MeshEQ : Node {
    AudioInlet upInlet;
    AudioInlet leftInlet;
    AudioInlet rightInlet;
    AudioInlet downInlet;
    AudioOutlet upOutlet;
    AudioOutlet leftOutlet;
    AudioOutlet rightOutlet;
    AudioOutlet downOutlet;
    MeshEQ() {
        
    }
};

};