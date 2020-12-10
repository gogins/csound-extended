#include "HarmonyIFS.hpp"

namespace csound {

    HarmonyInterpolationPoint::HarmonyInterpolationPoint() {}
    
    HarmonyInterpolationPoint::HarmonyInterpolationPoint(const HarmonyInterpolationPoint &other) {
        *this = other;
    }
    
    HarmonyInterpolationPoint::HarmonyInterpolationPoint(double t_, double P_, double I_, double T_, double s_P_, double s_I_, double s_T_) {
        initialize();
        set_t(t_);
        set_P(P_);
        set_I(I_);
        set_T(T_);
        set_s_P(s_P_);
        set_s_I(s_I_);
        set_s_T(s_T_);
    }
    
    HarmonyInterpolationPoint::~HarmonyInterpolationPoint() {
        initialize();
    }
    
    void HarmonyInterpolationPoint::initialize() {
        resize(HIP_ELEMENT_COUNT);
        operator *= (0);
        (*this)[HIP_HOMOGENEITY] = 1.0;
    }
    
    double HarmonyInterpolationPoint::get_t() const {
        return (*this)[HIP_TIME];
    }
    
    double HarmonyInterpolationPoint::get_P() const {
        return (*this)[HIP_PRIME_FORM];
    }
    
    double HarmonyInterpolationPoint::get_I() const {
        return (*this)[HIP_INVERSION];
    }
    
    double HarmonyInterpolationPoint::get_T() const {
        return (*this)[HIP_TRANSPOSITION];
    }
    
    double HarmonyInterpolationPoint::get_s_P() const {
        return (*this)[HIP_PRIME_FORM_SCALING];
    }
    
    double HarmonyInterpolationPoint::get_s_I() const {
        return (*this)[HIP_INVERSION_SCALING];
    }
    
    double HarmonyInterpolationPoint::get_s_T() const {
        return (*this)[HIP_TRANSPOSITION_SCALING];
    }
    
    void HarmonyInterpolationPoint::set_t(double value) {
        (*this)[HIP_TIME] = value;
    }
    
    void HarmonyInterpolationPoint::set_P(double value) {
        (*this)[HIP_PRIME_FORM] = value;
    }
    
    void HarmonyInterpolationPoint::set_I(double value) {
        (*this)[HIP_INVERSION] = value;
    }
    
    void HarmonyInterpolationPoint::set_T(double value) {
        (*this)[HIP_TRANSPOSITION] = value;
    }
    
    void HarmonyInterpolationPoint::set_s_P(double value) {
        (*this)[HIP_PRIME_FORM_SCALING] = value;
    }
    
    void HarmonyInterpolationPoint::set_s_I(double value) {
        (*this)[HIP_INVERSION_SCALING] = value;
    }
    
    void HarmonyInterpolationPoint::set_s_T(double value) {
        (*this)[HIP_TRANSPOSITION_SCALING] = value;
    }  

    std::string HarmonyInterpolationPoint::toString() const {
        char buffer[0x1000];
        std::snprintf(buffer, 0x1000, "t: %9.4f P: %9.4f I: %9.4f T: %9.4f s_P: %9.4f s_I: %9.4f s_T: %9.4f 1: %9.4f\n", 
            get_t(), get_P(), get_I(), get_T(), get_s_P(), get_s_I(), get_s_T(), (*this)[HIP_HOMOGENEITY]);
        return buffer;
    }
    
    HarmonyIFS::HarmonyIFS() {};

    HarmonyIFS::~HarmonyIFS() {};
    
    PITV &HarmonyIFS::get_pitv() {
        return pitv;
    }

    ///    harmony_ifs.initialize(4, 60., 48., true, true, .02);
    void HarmonyIFS::initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlaps_, bool rescale_, double g_) {
        voices = voices_;
        range = range_;
        bass = bass_;
        note_duration = note_duration_;
        tie_overlaps = tie_overlaps_;
        rescale = rescale_;
        g = g_;
        pitv.initialize(voices_, range_, g_, true);
        interpolation_points.clear();
    }
    
    HarmonyInterpolationPoint HarmonyIFS::add_interpolation_point(double t, double P, double I, double T, double s_P, double s_I, double s_T) {
        HarmonyInterpolationPoint harmony_interpolation_point = HarmonyInterpolationPoint(t, P, I, T, s_P, s_I, s_T);
        interpolation_points.push_back(harmony_interpolation_point);
        return harmony_interpolation_point;        
    }
    
    static bool interpolation_point_less(const HarmonyInterpolationPoint &a, const HarmonyInterpolationPoint &b) {
        if (a.get_t() < b.get_t()) {
            return true;
        } else {
            return false;
        }
    };

    void HarmonyIFS::initialize_hutchinson_operator() {
        hutchinson_operator.clear();
        std::sort(interpolation_points.begin(), interpolation_points.end(), interpolation_point_less);
        HarmonyInterpolationPoint p_0 = interpolation_points.front();
        HarmonyInterpolationPoint p_N = interpolation_points.back();
        for (int i = 1, n = interpolation_points.size(); i < n; ++i) {
            HarmonyInterpolationPoint p_i_1 = interpolation_points[i - 1];
            HarmonyInterpolationPoint p_i = interpolation_points[i];
            Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(8, 8);
            // t or time dimension.
            transformation(0, 0) = (p_i.get_t() - p_i_1.get_t()) / (p_N.get_t() - p_0.get_t());
            transformation(0, 7) = ((p_N.get_t() * p_i_1.get_t()) - (p_0.get_t() * p_i.get_t())) / (p_N.get_t() - p_0.get_t());
            // P or prime-form dimension.
            transformation(1, 0) = ((p_i.get_P() - p_i_1.get_P()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_P() * ((p_N.get_P() - p_0.get_P()) / (p_N.get_t() - p_0.get_t())));
            transformation(1, 1) = p_i.get_s_P();
            transformation(1, 7) = (((p_N.get_t() * p_i_1.get_P()) - (p_0.get_t() * p_i.get_P())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_P() * (((p_i.get_t() * p_0.get_P()) - (p_0.get_t() * p_N.get_P())) / (p_N.get_t() - p_0.get_t())));
            // I or inversion dimension.
            transformation(2, 0) = ((p_i.get_I() - p_i_1.get_I()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_I() * ((p_N.get_I() - p_0.get_I()) / (p_N.get_t() - p_0.get_t())));
            transformation(2, 2) = p_i.get_s_I();
            transformation(2, 7) = (((p_N.get_t() * p_i_1.get_I()) - (p_0.get_t() * p_i.get_I())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_I() * (((p_i.get_t() * p_0.get_I()) - (p_0.get_t() * p_N.get_I())) / (p_N.get_t() - p_0.get_t())));
            // T or transposition dimension.
            transformation(3, 0) = ((p_i.get_T() - p_i_1.get_T()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_T() * ((p_N.get_T() - p_0.get_T()) / (p_N.get_t() - p_0.get_t())));
            transformation(3, 3) = p_i.get_s_T();
            transformation(3, 7) = (((p_N.get_t() * p_i_1.get_T()) - (p_0.get_t() * p_i.get_T())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_T() * (((p_i.get_t() * p_0.get_T()) - (p_0.get_t() * p_N.get_T())) / (p_N.get_t() - p_0.get_t())));
            hutchinson_operator.push_back(transformation);
        }
    }
    
    HarmonyEvent HarmonyIFS::point_to_note(const HarmonyPoint &point) {
        HarmonyEvent event;
        event.note.setTime(point.get_t());
        event.note.setDuration(note_duration);
        event.note.setStatus(144);
        event.note.setInstrument(point.get_i());
        int P = std::round(point.get_P());
        int I = std::round(point.get_I());
        int T = std::round(point.get_T());
        event.chord = pitv.toChord(P, I, T, 0)[2];
        event.note.setKey(point.get_k());
        event.note.setVelocity(point.get_v());
        return event;
    }
    
    void HarmonyIFS::remove_duplicate_notes() {
        System::inform("HarmonyIFS::remove_duplicate_notes: before: %d events...\n", score_attractor.size());
        std::map<std::string, Event> unique_events;
        for (auto &event : score) {
            unique_events[event.toString()] = event;
        }
        score.clear();
        for (auto &event : unique_events) {
            score.push_back(event.second);
        }
        System::inform("                                    after:  %d events.\n", score_attractor.size());
    }
    
    void HarmonyIFS::generate_score_attractor(int depth) {
        System::inform("HarmonyIFS::generate_score_attractor: depth:  %d...\n", depth);
        score_attractor.clear();
        int iteration = 0;
        HarmonyPoint initial_point;
        initial_point.set_k(60.);
        initial_point.set_v(60.);
        initial_point.set_i(1);
        for (auto &transformation : hutchinson_operator) {
            std::cerr << transformation << std::endl;
        }
        iterate(depth, iteration, initial_point);
        System::inform("                                      points: %d.\n", score.size());
        translate_score_attractor_to_score();
    }
    
    void HarmonyIFS::iterate(int depth, int iteration, HarmonyPoint &p) {
        iteration = iteration + 1;
        if (iteration >= depth) {
            HarmonyEvent event = point_to_note(p);
            score_attractor.push_back(event);
            return;
        }
        for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
            const Eigen::MatrixXd &T = hutchinson_operator[i];
            p = T * p;
            iterate(depth, iteration, p);
        }
    }
    
    void HarmonyIFS::translate_score_attractor_to_score() {
        System::inform("HarmonyIFS::translate_score_attractor_to_score...\n");
        score.clear();
        System::inform("  Rescaling, tempering, and conforming notes...\n");
        double minimum_time = score_attractor.front().note.getTime();
        double minimum_key = score_attractor.front().note.getKey();
        double maximum_key = minimum_key;
        for (auto &event : score_attractor) {
            if (minimum_time > event.note.getTime()) {
                minimum_time = event.note.getTime();
            }
            if (minimum_key > event.note.getKey()) {
                minimum_key = event.note.getKey();
            }
            if (maximum_key < event.note.getKey()) {
                maximum_key = event.note.getKey();
            }
        }
        double key_range = maximum_key - minimum_key;
        rescale = 1.;
        if (key_range != 0.) {
            rescale = range / key_range;
        }
        for (auto &event : score_attractor) {
            double time_ = event.note.getTime();
            time_ = time_ - minimum_time;
            event.note.setTime(time_);
            double key = event.note.getKey();
            key = key - minimum_key;
            key = key * rescale;
            key = key + minimum_key + bass;
            event.note.setKey(key);   
            event.note.temper(12.);
            conformToChord(event.note, event.chord);
            score.push_back(event.note);
        }
        System::inform("  Removing duplicate notes...\n");
        remove_duplicate_notes();
        System::inform("  Tieing overlapping notes...\n");
        System::inform("HarmonyIFS::tie overlapping notes: before: %d events...\n", score.size());
        score.tieOverlappingNotes();
        System::inform("                                    after: %d events.\n", score.size());
        score_attractor.clear();
        System::inform("  Finished translating score attractor to final score.\n");
    }
    
    int HarmonyIFS::get_transformation_count() const {
        return hutchinson_operator.size();
    }

    void HarmonyIFS::set_transformation(int transformation, int row, int column, double value) {
        hutchinson_operator[transformation](row, column) = value;
    }

}