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
#ifndef NODE_HPP
#define NODE_HPP

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
    % {
#include "Score.hpp"
#include <vector>
#include <eigen3/Eigen/Dense>
#include <functional>
%
}
%include "std_string.i"
%include "std_vector.i"
%template(NodeVector) std::vector<csound::Node*>;
#else
#include "Score.hpp"
#include <vector>
#include <eigen3/Eigen/Dense>
#include <functional>
#endif

namespace csound
{

/**
 * Base class for all music graph nodes in the Silence system.
 * Nodes can transform silence::Events produced by child nodes.
 * Nodes can generate silence::Events.
 */
class SILENCE_PUBLIC Node
{
protected:
    Eigen::MatrixXd localCoordinates;
public:
    /**
     * Child Nodes, if any.
     */
    std::vector<Node *> children;
    Node();
    virtual ~Node();
    /**
     * Returns the local transformation of coordinate system.
     */
    virtual Eigen::MatrixXd getLocalCoordinates() const;
    /**
     * The default implementation postconcatenates its own local coordinate
     * system with the global coordinates, then passes the score and the
     * product of coordinate systems to each child, thus performing a
     * depth-first traversal of the music graph. In case a derived class needs
     * to apply a different local transformation to each child node's notes,
     * this method must be overridden.
     */
    virtual void traverse(const Eigen::MatrixXd &globalCoordinates,
                          Score &score);
    /**
     * Optionally generate notes into the score. The notes must be produced at
     * the coordinate system with origin zero, and will automatically be
     * transformed to the global coordinate system.
     */
    virtual void generate(Score &score);
    /**
     * Optionally transform any or all notes produced by child nodes of this,
     * which are in the score, in the global coordinate system. The default
     * implementation does nothing. Additional notes may also be generated.
     */
    virtual void transform(Score &score);
    /**
     * Returns identity.
     */
    virtual Eigen::MatrixXd createTransform();
    virtual void clear();
    /**
     * Returns a reference to the indicated element of the local transformation of coordinate system.
     */
    virtual double &element(size_t row, size_t column);
    /**
     * Sets the indicated element of the local transformation of coordinate system.
     */
    virtual void setElement(size_t row, size_t column, double value);
    virtual void addChild(Node *node);
};
typedef Node* NodePtr;

/**
 * Removes all duplicate events produced by the child nodes of this.
 */
class SILENCE_PUBLIC RemoveDuplicates : public Node
{
public:
    virtual void transform(Score &score);
};

/**
 * Node that uses any callable to implement Node::transform.
 * This is particularly useful as the callable may be a closure that
 * refers to objects outside of the music graph.
 */
class SILENCE_PUBLIC Transformer : public Node
{
public:
    std::function<void(csound::Score &)> callable;
    virtual void transform(Score &score) {
        callable(score);
    }

};

/**
 * Node that uses any callable to implement Node::generate.
 * This is particularly useful as the callable may be a closure that
 * refers to objects outside of the music graph.
 */
class SILENCE_PUBLIC Generator : public Node
{
public:
    std::function<void(csound::Score &)> callable;
    virtual void generate(Score &score) {
        callable(score);
    }

};
}
#endif
