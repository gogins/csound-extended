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
#include "CppSound.hpp"
#include "MCRM.hpp"
#include "dkm.hpp"

namespace csound
{
MCRM::MCRM() : depth(0)
{
}

MCRM::~MCRM()
{
}

void MCRM::setDepth(int depth)
{
    this->depth = depth;
}

void MCRM::resize(size_t count)
{
    for(size_t i = 0; i < count; i++)
    {
        transformations.push_back(createTransform());
    }
    weights.resize(transformations.size(), transformations.size());
    for(size_t precursor = 0; precursor < transformations.size(); precursor++)
    {
        for(size_t successor = 0; successor < transformations.size(); successor++)
        {
            weights(precursor,successor) = 1.0;
        }
    }
}

void MCRM::setTransformationElement(size_t index, size_t row, size_t column, double value)
{
    transformations[index](row,column) = value;
}

void MCRM::setWeight(size_t precursor, size_t successor, double weight)
{
    weights(precursor,successor) = weight;
}

void MCRM::iterate(int d, size_t p, const Event &event, double weight)
{
    d--;
    if(d < 0)
    {
        double velocity = event.getVelocity() * weight;
        if(velocity > 0.0)
        {
            score.push_back(event);
        }
    }
    else
    {
        for(size_t s = 0; s < transformations.size(); s++)
        {
            const Eigen::MatrixXd &t = transformations[s];
            Eigen::VectorXd ev = t * event;
            Event e = ev;
            double w = 0.0;
            if(weight == -1.0)
            {
                w = 1.0;
            }
            else
            {
                w = weights(p, s) * weight;
            }
            iterate(d, s, e, w);
        }
    }
}

void MCRM::generate()
{
    Event event;
    event.setStatus(144);
    event.setVelocity(64);
    event.setKey(60);
    event.setDuration(1);
    double weight = -1;
    iterate(depth, 0, event, weight);
}

void MCRM::produceOrTransform(Score &score,
                              size_t beginAt,
                              size_t endAt,
                              const Eigen::MatrixXd &coordinates)
{
    generate();
    ScoreNode::produceOrTransform(score, beginAt, endAt, coordinates);
}

KMeansMCRM::KMeansMCRM() :
    algorithm_type(RANDOM),
    sample_count(2000000),
    means_count(2000)
{
}

KMeansMCRM::~KMeansMCRM()
{
}

void KMeansMCRM::generate()
{
    if (algorithm_type == RANDOM) {
        random_algorithm();
    } else if (algorithm_type == DETERMINISTIC) {
        deterministic_algorithm();
    }
    means_to_notes();
}

void KMeansMCRM::random_algorithm()
{
}

void KMeansMCRM::iterate(int d, size_t p, const Event &event, double weight)
{
    d--;
    if (samples.size() >= sample_count) {
        return;
    }
    if(d < 0)
    {
        double velocity = event.getVelocity() * weight;
        if(velocity > 0.0)
        {
            samples.push_back({event[0], event[1], event[3], event[4], event[5], event[6]});
        }
    }
    else
    {
        for(size_t s = 0; s < transformations.size(); s++)
        {
            const Eigen::MatrixXd &t = transformations[s];
            Eigen::VectorXd ev = t * event;
            Event e = ev;
            double w = 0.0;
            if(weight == -1.0)
            {
                w = 1.0;
            }
            else
            {
                w = weights(p, s) * weight;
            }
            iterate(d, s, e, w);
        }
    }
}


void KMeansMCRM::deterministic_algorithm()
{
    System::inform("KMeansMCRM::deterministic_algorithm...\n");
    Event event;
    event.setStatus(144);
    event.setVelocity(64);
    event.setKey(60);
    event.setDuration(1);
    double weight = -1;
    iterate(depth, 0, event, weight);
    System::inform("KMeansMCRM::deterministic_algorithm.\n");
}

static Event mean_to_note(const std::array<float, KMeansMCRM::MEASURE_DIMENSIONS> &mean)
{
    Event event;
    event.setTime(mean[0]);
    event.setDuration(mean[1]);
    event.setStatus(144);
    event.setInstrument(mean[2]);
    event.setKey(mean[3]);
    event.setVelocity(mean[4]);
    event.setPan(mean[5]);
    return event;
}

void KMeansMCRM::means_to_notes()
{
    System::inform("KMeansMCRM::means_to_notes...\n");
    System::inform("dkm::kmeans_lloyd...\n");
    auto clusters = dkm::kmeans_lloyd(samples, MEASURE_DIMENSIONS);
    auto &means = std::get<0>(clusters);
    score.clear();
    for (size_t i = 0, n = means.size(); i < n; ++i) {
        score.append(mean_to_note(means[i]));
    }
    System::inform("dkm::kmeans_lloyd.\n");
    System::inform("KMeansMCRM::means_to_notes.\n");
}

}
