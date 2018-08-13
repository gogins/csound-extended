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
#include "Node.hpp"
#include <set>

namespace csound
{
Node::Node()
{
    localCoordinates.resize(Event::ELEMENT_COUNT, Event::ELEMENT_COUNT);
    localCoordinates = createTransform();
}

Node::~Node()
{
}

Eigen::MatrixXd Node::createTransform()
{
    Eigen::MatrixXd matrix = Eigen::MatrixXd::Identity(Event::ELEMENT_COUNT, Event::ELEMENT_COUNT);
    return matrix;
}

Eigen::MatrixXd Node::getLocalCoordinates() const
{
    return localCoordinates;
}

void Node::traverse(const Eigen::MatrixXd &global_coordinates,
                    Score &global_score)
{
    // Obtain the composite transformation of coordinate system
    // by post-concatenating the local transformation of coordinate system
    // with the global, or enclosing, transformation of coordinate system.
    Eigen::MatrixXd composite_coordinates = getLocalCoordinates() * global_coordinates;
    // Descend into each of the child nodes.
    Score score_from_children;
    for(size_t i = 0, n = children.size(); i < n; ++i) {
        children[i]->traverse(composite_coordinates, score_from_children);
    }
    // Optionally transform any or all notes produced by the child nodes,
    // which are in the composite coordinate system.
    transform(score_from_children);
    // Add the child notes to the global score.
    global_score.insert(global_score.end(), score_from_children.begin(), score_from_children.end());
    // Optionally generate new notes in the coordinate system with origin at zero.
    Score score_from_this;
    generate(score_from_this);
    // Then transform these new notes to the composite coordinate system.
    score_from_this.transform(composite_coordinates);
    // Add the generated notes to the global score.
    global_score.insert(global_score.end(), score_from_this.begin(), score_from_this.end());
}

void Node::generate(Score &score)
{
}

void Node::transform(Score &score)
{
}

void Node::clear()
{
    Node *node = 0;
    for(std::vector<Node*>::iterator i = children.begin(); i != children.end(); ++i) {
        node = *i;
        node->clear();
    }
    children.clear();
}

double &Node::element(size_t row, size_t column)
{
    return localCoordinates(row, column);
}

void Node::setElement(size_t row, size_t column, double value)
{
    localCoordinates(row, column) = value;
}

void Node::addChild(Node *node)
{
    children.push_back(node);
}

void RemoveDuplicates::transform(Score &collectingScore)
{
    std::set<std::string> uniqueEvents;
    Score newScore;
    for (size_t i = 0, n = collectingScore.size(); i < n; ++i) {
        const Event &event = collectingScore[i];
        std::string istatement = event.toCsoundIStatement();
        if (uniqueEvents.find(istatement) == uniqueEvents.end()) {
            newScore.push_back(event);
            uniqueEvents.insert(istatement);
        }
    }
    collectingScore = newScore;
}

}
