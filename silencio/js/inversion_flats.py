'''
INVERSION FLATS

Michael Gogins
16 August 2020

Prototype code for computing hyperplane equations for inversion flats that 
will generate OPTI chords from non-eI OPT chords.

I have coded several methods in order to develop my understanding of what is 
going on and how best to determine the inversion flat and implement the 
reflection.
'''
print(__doc__)

import copy
import math
import random
import mpmath
import scipy
import scipy.linalg
import scipy.spatial
import scipy.spatial.distance
import sys

print(sys.version)

print("scipy:", scipy.__version__)

debug_ = False
def debug(*args):
    if debug_ == True:
        print(*args)

print("\nSee 'generalized cross product' from https://math.stackexchange.com/questions/2723294/how-to-determine-the-equation-of-the-hyperplane-that-contains-several-points and https://madoshakalaka.github.io/2019/03/02/generalized-cross-product-for-high-dimensions.html...\n")

def normalized_vector(a, b):
    vector = scipy.subtract(a, b)
    norm = scipy.linalg.norm(vector)
    normalized_vector_ = scipy.divide(vector, norm)
    return normalized_vector_
    
def center(n):
    center_ = []
    g = 12. / n
    for i in range(n):
        center_.append(i * g)
    return center_
    
def cyclical_region_vertices(n, transpositional_equivalence=False):
    vertices = []
    for i in range(n):
        vertex = []
        for j in range(n):
            if j > i:
                vertex.append(12)
            else:
                vertex.append(0)
        if transpositional_equivalence == True:
            vertices.append(eT(vertex))
        else:
            vertices.append(vertex)
    print("vertices:")
    vertices.sort()
    for vertex in vertices:
        print(vertex)
    return vertices
    
def midpoint(a, b):
    midpoint_ = []
    for i in range(len(a)):
        midpoint_.append((a[i] + b[i]) / 2)
    return midpoint_
    
def eT(chord):
    sum_ = 0.;
    for i in range(len(chord)):
        sum_ = sum_ + chord[i]
    transposition = sum_ / len(chord)
    eT_ = []
    for i in range(len(chord)):
        eT_.append(chord[i] - transposition)
    return eT_
    
def eOPT(chord):
    pass
    
def ceiling(chord):
    ceiling_ = copy.deepcopy(chord)
    for i in range(len(chord)):
        ceiling_[i] = math.ceil(chord[i])
    return ceiling_
    
def eTT(chord):
    et_ = eT(chord);
    et_ceiling = ceiling(et_)
    while sum(et_ceiling) < 0:
     return et_ceiling;
    
def generalized_cross_product(vectors):
    dim = len(vectors[0])
    product = []
    for j in range(dim):
        basis_vector = [0] * dim
        basis_vector[j] = 1
        debug("basis_vector:", basis_vector)
        matrix = scipy.vstack([vectors, basis_vector])
        debug("matrix:", matrix)
        debug("determinant of matrix:", scipy.linalg.det(matrix))
        product.append(scipy.linalg.det(matrix))
    return product
    
def hyperplane_equation_by_cross_product(points, t_equivalence = 'True'):
    vectors = []
    t_ = []
    if t_equivalence == True:
        debug("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    debug("points:", points)
    debug("determinant of points:", scipy.linalg.det(points))
    subtrahend = points[-1]
    debug("subtrahend:", subtrahend)
    for i in range(len(points) - 1):
        vector = scipy.subtract(points[i], subtrahend)
        debug("vector[", i, "]:", vector)
        vectors.append(vector)
    debug("vectors:", vectors)
    product = generalized_cross_product(vectors) 
    print("generalized_cross_product:", product)
    norm = scipy.linalg.norm(product)
    debug("norm:", norm)
    unit_normal_vector = scipy.divide(product, norm)
    print("unit_normal_vector:")
    for e in unit_normal_vector:
        print(e)
    constant_term = scipy.dot(scipy.transpose(unit_normal_vector), subtrahend)
    print("constant_term:", constant_term)
    return unit_normal_vector, constant_term
    
def hyperplane_equation_by_nullspace_from_vectors(points, t_equivalence = 'True'):
    vectors = []
    t_ = [] 
    if t_equivalence == True:
        debug("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    debug("points:", points)
    try:
        debug("determinant of points:", scipy.linalg.det(points))
    except:
        pass
    subtrahend = points[-1]
    debug("subtrahend:", subtrahend)
    for i in range(len(points) - 1):
        vector = scipy.subtract(points[i], subtrahend)
        debug("vector[", i, "]:", vector)
        vectors.append(vector)
    nullspace = scipy.linalg.null_space(vectors)
    debug("nullspace from vectors:", nullspace)
    norm = scipy.linalg.norm(nullspace)
    debug("norm:", norm)
    unit_normal_vector = scipy.divide(nullspace, norm)
    print("unit_normal_vector:")
    for e in unit_normal_vector:
        print(e)
    constant_term = scipy.dot(scipy.transpose(unit_normal_vector), subtrahend)
    print("constant_term:", constant_term)
    return unit_normal_vector, constant_term

def hyperplane_equation_by_nullspace_from_points(points, t_equivalence = 'True'):
    t_ = []
    if t_equivalence == True:
        debug("original points:\n", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    debug("points:\n", points)
    try:
        debug("determinant of points:", scipy.linalg.det(points))
    except:
        pass
    homogeneous_points = []
    for point in points:
        homogeneous_points.append(scipy.append(point, [1]))
    debug("homogeneous points:", homogeneous_points)
    nullspace = scipy.linalg.null_space(homogeneous_points)
    nullspace = scipy.ndarray.flatten(nullspace[:-1])
    debug("nullspace from homogeneous points:", nullspace)
    constant_term = scipy.dot(scipy.transpose(nullspace), points[0])
    debug("constant_term:", constant_term)
    norm = scipy.linalg.norm(nullspace)
    debug("norm:", norm)
    unit_normal = scipy.divide(nullspace, norm)
    print("unit normal vector:", unit_normal)
    constant_term = scipy.dot(scipy.transpose(unit_normal), points[0])
    print("constant_term:", constant_term)
    return unit_normal, constant_term
    
def hyperplane_equation_by_svd_from_vectors(points, t_equivalence = 'True'):
    t_ = []
    if t_equivalence == True:
        debug("original points:\n", points)
        for point in points:
            t_.append(eT(point))
        points = t_
    debug("points:\n", points)
    vectors = []
    subtrahend = points[-1]
    debug("subtrahend:", subtrahend)
    for i in range(len(points) - 1):
        vector = scipy.subtract(points[i], subtrahend)
        vectors.append(vector)
    # debug("vectors:\n", vectors)
    vectors = scipy.array(vectors)
    debug("vectors:\n", vectors)
    U, singular_values, V = scipy.linalg.svd(vectors)    
    debug("U:\n", U)
    print("singular values:", singular_values)
    debug("V:\n", V)
    normal_vector = V[-1]
    debug("normal_vector:", normal_vector)
    norm = scipy.linalg.norm(normal_vector)
    debug("norm:", norm)
    unit_normal_vector = scipy.divide(normal_vector, norm)
    print("unit_normal_vector:")
    for e in unit_normal_vector:
        print(e)
    constant_term = scipy.dot(scipy.transpose(unit_normal_vector), subtrahend)
    print("constant_term:", constant_term)
    print()
    return unit_normal_vector, constant_term
    
#~ def hyperplane_equation_by_svd_from_points(points, t_equivalence = 'True'):
    #~ global debug_
    #~ debug_ = True
    #~ t_ = []
    #~ if t_equivalence == True:
        #~ debug("original points:", points)
        #~ for point in points:
            #~ t_.append( eT(point))
        #~ points = t_
    #~ temp = []
    #~ for point in points:
        #~ temp.append(scipy.append(point, [1]))
    #~ points = temp
    #~ debug("points:", points)
    #~ left_singular_vectors, singular_values, right_singular_vectors = scipy.linalg.svd(points)    
    #~ debug("left singular vectors:", left_singular_vectors)
    #~ debug("singular values:", singular_values)
    #~ debug("right singular vectors:", right_singular_vectors)
    #~ minimum_singular_value = min(singular_values)
    #~ index_ = list(singular_values).index(minimum_singular_value)
    #~ # There aren't enough singular values, so I am assuming the last singular 
    #~ # vector is the one required.
    #~ normal_vector = right_singular_vectors[-1]
    #~ debug("normal_vector:", normal_vector)
    #~ norm = scipy.linalg.norm(normal_vector)
    #~ debug("norm:", norm)
    #~ unit_normal_vector = scipy.divide(normal_vector, norm)
    #~ print("unit_normal_vector:", scipy.ndarray.flatten(unit_normal_vector))
    #~ constant_term = scipy.dot(scipy.transpose(unit_normal_vector), points[0])
    #~ print("constant_term:", constant_term)
    #~ debug_ = False
    #~ return unit_normal_vector, constant_term
    
#~ def hyperplane_equation_by_least_squares(points, t_equivalence = 'True'):
    #~ global debug_
    #~ debug_ = True
    #~ t_ = []
    #~ if t_equivalence == True:
        #~ debug("original points:", points)
        #~ for point in points:
            #~ t_.append( eT(point))
        #~ points = t_
    #~ vectors = []
    #~ subtrahend = points[-1]
    #~ debug("subtrahend:", subtrahend)
    #~ for i in range(len(points) - 1):
        #~ vector = scipy.subtract(points[i], subtrahend)
        #~ debug("vector[", i, "]:", vector)
        #~ vectors.append(vector)
    #~ temp = []
    #~ for point in points:
        #~ temp.append(scipy.append(point, [1]))
    #~ points = temp
    #~ debug("vectors:", vectors)
    #~ zeros = [0] * len(vectors)
    #~ fit = scipy.linalg.lstsq(vectors, zeros)
    #~ debug("solution:", fit[0])
    #~ debug("residues:", fit[1])
    #~ debug("rank:", fit[2])
    #~ debug("singular values:", fit[3])
    #~ debug_ = False
    #~ return fit

'''
Returns the scalar hyperplane equation, in the form of a unit normal vector 
and constant term, for the first fundamental domain of OPT equivalence for n 
dimensions. The unit normal vector can be rotated from sector to sector by 
permuting its terms, and the constant term is the dot product of the unit 
normal and the center.
'''
def hyperplane_equation_from_dimensonality(dimensions, transpositional_equivalence=False, sector=1):
    center_ = center(dimensions)
    cyclical_region_vertices_ = cyclical_region_vertices(dimensions, transpositional_equivalence)
    for i in range(sector):
        front = cyclical_region_vertices_.pop(0);
        cyclical_region_vertices_.append(front);
    upper_point = midpoint(cyclical_region_vertices_[1], cyclical_region_vertices_[2])
    lower_point = midpoint(cyclical_region_vertices_[0], cyclical_region_vertices_[-1])
    print("sector:", sector)
    print("upper_point:", upper_point)
    print("lower_point:", lower_point)
    if transpositional_equivalence == True:
        center_ = eT(center_)
        #~ lower_point = eT(lower_point)
        #~ upper_point = eT(upper_point)
    normal_vector = scipy.subtract(upper_point, lower_point)
    norm = scipy.linalg.norm(normal_vector)
    unit_normal_vector = scipy.divide(normal_vector, norm)
    constant_term = unit_normal_vector.dot(center_)
    debug("hyperplane_equation_from_dimensonality for", dimensions, "voices:")
    print("center:")
    for e in center_:
        print(e)
    print("unit_normal_vector:")
    for e in unit_normal_vector:
        print(e)
    print("constant_term:", constant_term)
    print()
    return unit_normal_vector, constant_term
    
def distance_to_origin(v, u, c):
    numerator = abs(scipy.dot(v, u) + c)
    denominator = scipy.linalg.norm(u)**2
    distance = numerator / denominator
    return distance
    
# Ref(v,c) = v - 2 {[(v . u) - c] / (u . u)} u.
'''
inline SILENCE_PUBLIC Eigen::VectorXd reflect(const Eigen::VectorXd &v, const Eigen::VectorXd &u, double c) {
    auto v_dot_u = v.dot(u);
    auto v_dot_u_minus_c = v_dot_u - c;
    auto u_dot_u = u.dot(u);
    auto quotient = v_dot_u_minus_c / u_dot_u;
    auto subtrahend = u * (2. * quotient);
    auto reflection = v - subtrahend;
    return reflection;
}
'''
def reflect(v, u, c):
    print("Reflect by vector math:")
    print("v:", v)
    print("u:", u)
    print("c:", c)
    v_dot_u = scipy.dot(v, u)
    print("v_dot_u:", v_dot_u)
    v_dot_u_minus_c = scipy.subtract(v_dot_u, c)
    print("v_dot_u_minus_c:", v_dot_u_minus_c)
    u_dot_u = scipy.dot(u, u)
    print("u_dot_u:", u_dot_u)
    quotient = scipy.divide(v_dot_u_minus_c, u_dot_u)
    print("quotient:", quotient)
    subtrahend = scipy.multiply(u, (2 * quotient))
    print("subtrahend:", subtrahend)
    reflection = scipy.subtract(v, subtrahend)
    print("reflection:", reflection)
    return reflection
    
def reflect1(v, u, c):
    print("Reflect by vector math variant 1:")
    c = 0
    center_ = eT(center(len(v)))
    print("center_:", center_)
    print("v:", v)
    v = scipy.subtract(v, center_)
    print("v:", v)
    print("u:", u)
    print("c:", c)
    v_dot_u = scipy.dot(v, u)
    print("v_dot_u:", v_dot_u)
    v_dot_u_minus_c = scipy.subtract(v_dot_u, c)
    print("v_dot_u_minus_c:", v_dot_u_minus_c)
    u_dot_u = scipy.dot(u, u)
    print("u_dot_u:", u_dot_u)
    quotient = scipy.divide(v_dot_u_minus_c, u_dot_u)
    print("quotient:", quotient)
    subtrahend = scipy.multiply((2 * quotient), u)
    print("subtrahend:", subtrahend)
    reflection = scipy.subtract(v, subtrahend)
    print("reflection:", reflection)
    reflection = scipy.add(reflection, center_)
    print("reflection:", reflection)
    return reflection
    
#~ '''
#~ Rotates a chord up or down a different fundamental domain of the cyclical 
#~ region.

#~ '''
#~ def cycle_domain(chord, direction):
    #~ if direction > 0:
        #~ chord.append(chord[0] + 12
        #~ chord.remove(0)
    #~ else direction by < 0:
    
def reflect_in_center(chord):
    print("chord:", chord)
    print("reflect in center:")
    center_ = eT(center(len(chord)))
    print("center:", center_)
    center_2 = scipy.multiply(center_, 2)
    print("2 * center:", center_2)
    reflected = scipy.subtract(center_2, chord)
    print("reflected:", reflected)
    return reflected
'''
Computes the Householder reflector matrix and applies it to the chord.
The transformation is: H(p) = p - 2 * u * (u^T * p).
The corresponding matrix H is: H = I - 2 * u * u^T.
All vectors are column vectors.
'''
def reflect_by_householder(v, u, c):
    print("Reflect by Householder:")
    print("chord:", v)
    print("Unit normal vector:", u)
    center_ = center(len(v))
    tensor_ = scipy.outer(u, u);
    print("tensor_:\n", tensor_)
    product_ = scipy.multiply(tensor_, 2)
    print("product_:", product_)
    identity_ = scipy.eye(len(v))
    print("identity_:\n", identity_)
    householder = scipy.subtract(identity_, product_);
    print("householder:\n", householder)
    reflected_voices = scipy.matmul(householder, v) 
    print("reflected_voices:", reflected_voices)
    translated_voices = scipy.subtract(v, center_)
    print("moved to origin:", translated_voices)
    reflected_translated_voices = scipy.matmul(householder, translated_voices) 
    print("reflected_translated_voices:", reflected_translated_voices)
    reflection = scipy.add(reflected_translated_voices, center_)
    print("moved from origin:", reflection)
    print("reflection by householder:", reflection)
    return reflection
    
    
test_points = []
test_points.append([  4,  0, -1,  0])
test_points.append([  1,  2,  3, -1])
test_points.append([  0, -1,  2,  0])
test_points.append([ -1,  1, -1,  1])

overdetermined_test_points = copy.deepcopy(test_points)
overdetermined_test_points.append(test_points[1])
print("overdetermined_test_points:", overdetermined_test_points)

print("\nHyperplane equation for test points by generalized cross product from vectors\n".upper())
print("Generalized cross product should be: [13, 8, 20, 57].")
u0, c0 = hyperplane_equation_by_cross_product(test_points, False)
print("\nHyperplane equation for test points by null space from vectors\n".upper())
u1, c1 = hyperplane_equation_by_nullspace_from_vectors(test_points, False)
print("\nHyperplane equation for test points by null space from points\n".upper())
u2, c2 = hyperplane_equation_by_nullspace_from_points(test_points, False)
print("\nHyperplane equation for test points by singular value decomposition from vectors\n".upper())
hyperplane_equation_by_svd_from_vectors(test_points, False)
#~ print("\nHyperplane equation for test points by singular value decomposition from points\n".upper())
#~ u3, c3 = hyperplane_equation_by_svd_from_points(test_points, False)
print("\nHyperplane equation for overdetermined test points by null space from vectors\n".upper())
u4, c4 = hyperplane_equation_by_nullspace_from_vectors(overdetermined_test_points, False)
print("\nHyperplane equation for overdetermined test points by null space from points\n".upper())
u5, c5 = hyperplane_equation_by_nullspace_from_points(overdetermined_test_points, False)
print("\nHyperplane equation for overdetermined test points by singular value decomposition from vectors\n".upper())
u5, c5 = hyperplane_equation_by_svd_from_vectors(overdetermined_test_points, False)
#~ print("\nHyperplane equation for test points by least squares\n".upper())
#~ hyperplane_equation_by_least_squares(overdetermined_test_points, False)

u = u5
c = c5
test_point = [3, 1, 0, -5]
print("\nRotation of test point by vector math\n".upper())
reflected_test_point = reflect(test_point, u, c)
re_reflected_test_point = reflect(reflected_test_point, u, c)
print(test_point, reflected_test_point, re_reflected_test_point)
print("\nRotation of test point by Householder reflector\n".upper())
reflected_test_point = reflect_by_householder(test_point, u, c)
re_reflected_test_point = reflect(reflected_test_point, u, c)
print(test_point, reflected_test_point, re_reflected_test_point)

#~ normal = scipy.subtract(eT([0, 0,0,6]), eT([0, 0, 6,6]))
#~ normal = scipy.subtract(eT([0, 0,3,6]), eT([0, 3, 6,9]))
#~ print("\nnormal:", normal)
#~ norm_ = scipy.linalg.norm(normal)
#~ print("norm:", norm_)
#~ unit_normal = normal / norm_
#~ print("unit_normal:", unit_normal)
#~ constant_term = scipy.dot(unit_normal, eT(points[0])) / norm_
#~ print("constant_term:", constant_term)
#~ u = unit_normal
#~ c = constant_term
#~ distance = distance_to_origin([0, 1,2,3], u, c)
#~ print("\ndistance to origin:", distance)
#~ print("type:", type(c))
#~ #c = 2.

#~ points = []
#~ points.append([0, 0, 0,  6])
#~ points.append([0, 0, 6,  6])
#~ points.append([0, 3, 6,  9])
#~ points.append([0, 1, 3,  7])
#~ #points.append([0, 2, 4,  8])

#~ points = []
#~ points.append([0, 0, 0,  0])
#~ points.append([0, 0, 0, 12])
#~ points.append([0, 1, 2,  9])
#~ points.append([0, 3, 6,  9])
#~ points.append([0, 0, 0,  7])

#~ # Try them all...

#~ points = []
#~ points.append([0, 0, 0,  0])
#~ points.append([0, 0, 0,  1])
#~ points.append([0, 0, 0,  2])
#~ points.append([0, 0, 0,  4])
#~ points.append([0, 0, 0,  5])
#~ points.append([0, 0, 0,  6])
#~ points.append([0, 0, 0,  7])
#~ points.append([0, 0, 0,  9])
#~ points.append([0, 0, 0, 10])
#~ points.append([0, 0, 0, 11])
#~ points.append([0, 0, 0, 12])

#~ points.append([0, 1, 2,  3])
#~ points.append([0, 1, 2,  4])
#~ points.append([0, 1, 2,  5])
#~ points.append([0, 1, 2,  6])
#~ points.append([0, 1, 2,  7])
#~ points.append([0, 1, 2,  8])
#~ points.append([0, 1, 2,  9])
#~ points.append([0, 1, 2, 10])
#~ points.append([0, 1, 2, 11])

#~ points.append([0, 2, 4,  6])
#~ points.append([0, 2, 4,  7])
#~ points.append([0, 2, 4,  8])
#~ points.append([0, 2, 4,  9])
#~ points.append([0, 2, 4, 10])

#~ points.append([0, 3, 6,  9])

points = {}

# From _Science_ draft Figure 6.4.5 (a):
points4 = []
points4.append([0,  0,  6,  6])
points4.append([0,  1,  6,  7])
points4.append([0,  0,  5,  6])
points4.append([0,  2,  6,  8])
points4.append([0,  1,  5,  7])
points4.append([0,  0,  4,  6])
points4.append([0,  3,  6,  9])
points4.append([0,  2,  5,  8])
points4.append([0,  1,  4,  7])
points4.append([0,  0,  3,  6])
points4.append([0,  4,  6, 10])
points4.append([0,  3,  5,  9])
points4.append([0,  2,  4,  8])
points4.append([0,  1,  3,  7])
points4.append([0,  0,  2,  6])
points4.append([0,  5,  6, 11])
points4.append([0,  4,  5, 10])
points4.append([0,  3,  4,  9])
points4.append([0,  2,  3,  8])
points4.append([0,  1,  2,  7])
points4.append([0,  0,  1,  6])
points4.append([0,  6,  6, 12])
points4.append([0,  5,  5, 11])
points4.append([0,  4,  4, 10])
points4.append([0,  3,  3,  9])
points4.append([0,  2,  2,  8])
points4.append([0,  1,  1,  7])
points4.append([0,  0,  0,  6])
points[4] = points4

# From _Science_ draft Figure 6.4.5 (c):
#~ points = []

#~ points.append([0,  0,  0,  0])
#~ points.append([1,  1,  1,  1])
#~ points.append([2,  2,  2,  2])
#~ points.append([0,  3,  6,  9])
#~ points.append([1,  4,  7, 10])

print('''
SAMPLE OPT CHORDS FOR 4 VOICES

OPT:   15 [  -6.0000000   -4.0000000    4.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    2.0000000   10.0000000 ] iseI: true
OPT:   16 [  -6.0000000   -3.0000000    3.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    3.0000000    9.0000000 ] iseI: true
OPT:   17 [  -6.0000000   -3.0000000    4.0000000    5.0000000 ] sum:    0.0000 span:   11.0000 chord type: [   0.0000000    1.0000000    4.0000000   11.0000000 ] iseI: false
OPT:   18 [  -6.0000000   -2.0000000    2.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    4.0000000    8.0000000 ] iseI: true
OPT:   19 [  -6.0000000   -2.0000000    3.0000000    5.0000000 ] sum:    0.0000 span:   11.0000 chord type: [   0.0000000    1.0000000    5.0000000   10.0000000 ] iseI: false''')

# From _Science_ draft Figure 6.4.6 (b):
points5 = []

# Vertices only.
points5.append([0,  6,  6,  6, 12])
points5.append([0,  0,  6, 12, 12])
points5.append([0,  0,  0,  0,  0])
points5.append([0,  1,  2,  3,  4]
)

#~ # All 12TET chord types in flat.
#~ ## points5.append([0,  6,  6,  6, 12])
#~ points5.append([0,  0,  0,  6,  6])
#~ points5.append([0,  5,  5,  5, 10])
#~ ## points5.append([0,  1,  6, 11, 12])
#~ points5.append([0,  1,  1,  2,  7])
#~ points5.append([0,  4,  4,  4,  8])
#~ ## points5.append([0,  4,  5,  6, 10])
#~ points5.append([0,  1,  2,  6,  8])
#~ ## points5.append([0,  2,  6, 10, 12])
#~ points5.append([0,  2,  2,  4,  8])
#~ points5.append([0,  3,  3,  3,  6])
#~ points5.append([0,  3,  4,  5,  8])
#~ ## points5.append([0,  3,  5,  7, 10])
#~ points5.append([0,  2,  4,  7,  9])
#~ ## points5.append([0,  3,  6,  9, 12])
#~ points5.append([0,  0,  3,  6,  9])
#~ points5.append([0,  2,  2,  2,  4])
#~ points5.append([0,  2,  3,  4,  6])
#~ points5.append([0,  2,  4,  6,  8])
#~ ## points5.append([0,  2,  5,  8, 10])
#~ points5.append([0,  2,  4,  6,  9])
#~ ## points5.append([0,  4,  6,  8, 12])
#~ points5.append([0,  2,  4,  8,  8])
#~ points5.append([0,  1,  1,  1,  2])
#~ points5.append([0,  1,  2,  3,  4])
#~ points5.append([0,  1,  3,  5,  6])
#~ points5.append([0,  1,  4,  7,  8])
#~ ## points5.append([0,  1,  5,  9, 10])
#~ points5.append([0,  1,  3,  4,  8])
#~ ## points5.append([0,  5,  6,  7, 12])
#~ points5.append([0,  1,  2,  7,  7])
#~ points5.append([0,  0,  0,  0,  0])
#~ points5.append([0,  0,  1,  2,  2])
#~ points5.append([0,  0,  2,  4,  4])
#~ points5.append([0,  0,  3,  6,  6])
#~ points5.append([0,  0,  4,  8,  8])
#~ ## points5.append([0,  0,  5, 10, 10])
#~ ## points5.append([0,  0,  6, 12, 12])
#~ points5.append([0,  0,  0,  0,  6])

points5.append(center(5))
points[5] = points5

points6 = []
points6.append(center(6))
points6.append([0,  6,  6,  6,  6, 12])
points6.append([0,  0,  0,  0,  6,  6])
points6.append([0,  0,  6,  6, 12, 12])
points6.append([0,  0,  0,  6,  6,  6])
points6.append([0,  0,  0,  0,  0,  0])
points[6] = points6

points7 = []
points7.append(center(7))
points7.append([0,  6,  6,  6,  6,  6, 12])
points7.append([0,  0,  6,  6,  6, 12, 12])
points7.append([0,  0,  0,  6, 12, 12, 12])
points7.append([0,  0,  0,  0,  0,  0,  0])
points[7] = points7

def hyperplane_equation_from_random_flat(dimensions, count, transpositional_equivalence=True):
    flat = []
    for i in range(count):
        chord = [0.] * dimensions
        for voice in range(math.floor(dimensions/2)):
            pitch = random.uniform(-6, 6)
            chord[voice] = -pitch
            chord[-voice-1] = pitch
        if transpositional_equivalence == True:
            chord = eT(chord)
        chord.sort()
        print(chord)
        flat.append(chord)
    return hyperplane_equation_by_svd_from_vectors(flat, transpositional_equivalence)
        
hyperplane_equations_for_dimensions = {}

def print_hyperplane_equation(dimensions, transpositional_equivalence=True):
    global points
    global hyperplane_equations_for_dimensions
    print("hyperplane equations for dimensionality".upper(), dimensions)
    print()
    if dimensions not in hyperplane_equations_for_dimensions:
        hyperplane_equations_for_dimensions[dimensions] = {}
    if dimensions in points:
        points_ = points[dimensions]
        print("Points in inversion flat:".upper())
        for point in points_:
            print(point)
        print()
        try:
            print("Using cross product:".upper())
            u, c = hyperplane_equation_by_cross_product(points_, transpositional_equivalence)
            hyperplane_equations_for_dimensions[dimensions]["Cross Product"] = (u, c)
            print()
        except:
            pass
        try:
            print("Using nullspace of vectors:".upper())
            u, c = hyperplane_equation_by_nullspace_from_vectors(points_, transpositional_equivalence)
            hyperplane_equations_for_dimensions[dimensions]["Nullspace of Vectors"] = (u, c)
            print()
        except:
            pass
        try:
            print("Using singular value decomposition of vectors:".upper())
            u, c = hyperplane_equation_by_svd_from_vectors(points_, transpositional_equivalence)
            hyperplane_equations_for_dimensions[dimensions]["Singular Value Decomposition"] = (u, c)
            print()
        except:
            pass
    try:
        print("Using singular value decomposition of random flat:".upper())
        u, c = hyperplane_equation_from_random_flat(dimensions, 100, transpositional_equivalence)
        hyperplane_equations_for_dimensions[dimensions]["Random Inversion Flat"] = (u, c)
        print()
    except:
        pass
    print("Using cyclical region of OPT:".upper())
    u, c = hyperplane_equation_from_dimensonality(dimensions, transpositional_equivalence, 0)  
    hyperplane_equations_for_dimensions[dimensions]["Cyclical Region"] = (u, c)
    print()
    
def print_hyperplane_equations(transpositional_equivalence=True):
    for dimensions in range(3, 13):
        print_hyperplane_equation(dimensions, transpositional_equivalence)
        
def test_hyperplane_equation(u, c, chord, equivalence_):
    print(u, c, chord)
    reflection = reflect(chord, u, c)
    if equivalence_ == 1:
        print("reflection should be OPT and invariant:")
    elif equivalence_ == 2:
        print("reflection should be OPT but not invariant:")
    elif equivalence == 3:
         print("reflection should be OP but not OPT:")
    for i in range(len(chord)):
        print("     %9.4f  %9.4f" % (chord[i], reflection[i]))
    print("sum: %9.4f  %9.4f" % (sum(chord), sum(reflection)))
    involution = reflect(reflection, u, c)
    print("should be an involution:")
    for i in range(len(chord)):
        print("     %9.4f  %9.4f" % (chord[i], involution[i]))
    print("sum: %9.4f  %9.4f" % (sum(chord), sum(involution)))
    print()
    
'''
Kinds of chords to test:
1. On the inversion flat that bisects OPT.
2. In OPT but off the inversion flat.
3. In OP but off OPT.
'''
test_chords = []
test_chords.append((center(3), 1))
test_chords.append(([0, 2, 7], 1))
test_chords.append(([0, 0, 6], 1))
test_chords.append(([0, 1, 4], 2))
test_chords.append(([0, 2, 5], 2))
test_chords.append(([-1, 2, 8], 2))
test_chords.append((center(4), 1))
test_chords.append(([0, 6, 6, 12], 1))
test_chords.append(([0, 1, 1, 4], 2))
test_chords.append(([0,  5,  5,  5, 10], 1))
test_chords.append(([0,  0,  0,  6,  6], 1))
test_chords.append(([0,  5,  5,  5, 10], 1))
test_chords.append(([0,  0,  0, 12, 12], 2))
test_chords.append((center(6), 1))
test_chords.append((center(7), 1))
test_chords.append((center(8), 1))
test_chords.append((center(9), 1))
test_chords.append((center(7), 1))
test_chords.append((center(10), 1))
test_chords.append((center(11), 1))
test_chords.append((center(12), 1))
    
def test_hyperplane_equations():
    global hyperplane_equations_for_dimensions
    global test_chords
    for chord in test_chords:
        n = len(chord[0])
        for name in hyperplane_equations_for_dimensions[n]:
            try:
                equation = hyperplane_equations_for_dimensions[n][name]
                print("Testing", name, "Dimensionality", n)
                print()
                chord_ = scipy.asarray(eT(chord[0]))
                test_hyperplane_equation(equation[0], equation[1], chord_, chord[1])
            except:
                print("Exception...")
                print()
        print()
print()
print_hyperplane_equations(False)
print()
print_hyperplane_equations(True)
print()
test_hyperplane_equations()

vertices = cyclical_region_vertices(6)
print("EA:", normalized_vector(vertices[0], vertices[4]))
print("AB:", normalized_vector(vertices[0], vertices[1]))
print("EB:", normalized_vector(vertices[1], vertices[4]))
print("DC:", normalized_vector(vertices[2], vertices[3]))
mAE = midpoint(vertices[0], vertices[4])
mAB = midpoint(vertices[0], vertices[1])
print("mAE-mAB", normalized_vector(mAE, mAB))
mED = midpoint(vertices[4], vertices[3])
mBC = midpoint(vertices[2], vertices[1])
print("mED-mBC", normalized_vector(mED, mBC))
print(normalized_vector([0, 6, 6], [0, 6, 12]))
print(normalized_vector([0, 6, 6, 6], [0, 0, 6, 12]))
print(midpoint([0, 6, 6, 6], [0, 0, 6, 12]))
print(midpoint([0, 0, 6, 6], [0, 6, 6, 12]))
print(center(5))
print('''
Try just reflecting in the central point, then rotating the reflection 
back into the original fundamental domain.
''')
#~ print(reflect_in_center(eT([0, 4, 8])))
CM7 = [-1, 0, 4, 7]
u, c = hyperplane_equation_from_dimensonality(4, False, 3)
print(reflect(CM7, u, c))
print()
print(reflect_by_householder(eT([0, 4, 7]), [-1/3,-1/3,2/3], 0))
print()
print(reflect_by_householder(eT([0, 4, 7]), [-1/3,2/3,-1/3], 0))
print()
print(reflect_by_householder(eT([0, 4, 7]), [2/3,-1/3,-1/3], 0))
print()
print(reflect_by_householder(eT([0, 3, 7]), [-1/3,-1/3,2/3], 0))
print()
print(reflect_by_householder(eT([0, 3, 7]), [2/3,-1/3,-1/3], 0))
print()
print(reflect_in_center(eT([0, 4, 7])))
print()
print(reflect(eT([0, 3, 7]), [-1/3,2/3,-1/3], scipy.dot([-1/3,2/3,-1/3], eT(center(3)))))
print()
print(reflect1(eT([0, 3, 7]), [-1/3,2/3,-1/3], scipy.dot([-1/3,2/3,-1/3], eT(center(3)))))
print()
print(reflect_by_householder(eT([0, 3, 7]), [-1/3,2/3,-1/3], 0))
print()

for i in range(3, 13):
    print("dimensions: %2d  %f %f" % (i, math.sin(math.pi/i), math.cos(math.pi/i)))

print(reflect([-4.0000000,8.0000000,8.0000000], [-0.4082483,0.8164966,-0.4082483], 0))
print(reflect([-4.0000000,8.0000000,8.0000000], [0.8164966,-0.4082483,-0.4082483], -4.8989795))
 