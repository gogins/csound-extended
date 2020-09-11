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

def center(n):
    center_ = []
    g = 12. / n
    for i in range(n):
        center_.append(i * g)
    return center_
    
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
and constant term, for the first fundamental domain of OPT equivalence for n dimensions.
'''
def hyperplane_equation_from_dimensonality(dimensions, transpositional_equivalence=False):
    center_ = center(dimensions)
    upper_point = []
    lower_point = []
    for i in range(dimensions):
        upper_point.append(0)
        lower_point.append(0)
    upper_point[-1] = 12
    upper_point[-2] = 6
    for i in range(1, dimensions):
        lower_point[i] = 6
    if transpositional_equivalence == True:
        center_ = eT(center_)
        lower_point = eT(lower_point)
        upper_point = eT(upper_point)
    normal_vector = scipy.subtract(upper_point, lower_point)
    norm = scipy.linalg.norm(normal_vector)
    unit_normal_vector = scipy.divide(normal_vector, norm)
    constant_term = unit_normal_vector.dot(center_)
    debug("hyperplane_equation_from_dimensonality for", dimensions, "voices:")
    debug("center:", center_)
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
def reflect(v, u, c):
    debug("Reflect by vector math:", v, " in ", u, c)
    v_dot_u = scipy.dot(v, u)
    debug("v_dot_u:", v_dot_u)
    v_dot_u_minus_c = scipy.subtract(v_dot_u, c)
    debug("v_dot_u_minus_c:", v_dot_u_minus_c)
    u_dot_u = scipy.dot(u, u)
    debug("u_dot_u:", u_dot_u)
    quotient = scipy.divide(v_dot_u_minus_c, u_dot_u)
    debug("quotient:", quotient)
    subtrahend = scipy.multiply((2 * quotient), u)
    debug("subtrahend:", subtrahend)
    reflection = scipy.subtract(v, subtrahend)
    debug("reflection:", reflection)
    return reflection
    
#~ def reflect_by_householder(v, u, c):
    #~ print("Reflect by Householder:", v, " in ", u, c)
    #~ distance = c #distance_to_origin([4,0,-1,0], u, c)
    
    #~ tensor_ = scipy.outer(u, u);
    #~ print("tensor_:", tensor_)
    #~ product_ = scipy.multiply(tensor_, 2)
    #~ print("product_:", product_)
    #~ identity_ = scipy.eye(len(v))
    #~ print("identity_:", identity_)
    #~ householder = scipy.subtract(identity_, tensor_);
    #~ print("householder:", householder)
    #~ translated_voices = scipy.subtract(v, distance)
    #~ print("translated_voices:", translated_voices)
    #~ reflected_translated_voices = scipy.matmul(householder, translated_voices) 
    #~ print("reflected_translated_voices:", reflected_translated_voices)
    #~ reflection = scipy.add(reflected_translated_voices, distance)
    #~ print("reflection by householder:", reflection)
    #~ return reflection
    
#~ for voices in range(3, 13):
    #~ hyperplane_equation_from_dimensonality(voices, False)
    #~ hyperplane_equation_from_dimensonality(voices, True)
    
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
#~ print("\nRotation of test point by Householder reflector\n".upper())
#~ reflected_test_point = reflect_by_householder(test_point, u, c)
#~ re_reflected_test_point = reflect(reflected_test_point, u, c)
#~ print(test_point, reflected_test_point, re_reflected_test_point)

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
points5.append([0,  6,  6,  6, 12])
points5.append([0,  0,  0,  6,  6])
points5.append([0,  5,  5,  5, 10])
points5.append([0,  1,  6, 11, 12])
points5.append([0,  1,  1,  2,  7])
points5.append([0,  4,  4,  4,  8])
points5.append([0,  4,  5,  6, 10])
points5.append([0,  1,  2,  6,  8])
points5.append([0,  2,  6, 10, 12])
points5.append([0,  2,  2,  4,  8])
points5.append([0,  3,  3,  3,  6])
points5.append([0,  3,  4,  5,  8])
points5.append([0,  3,  5,  7, 10])
points5.append([0,  2,  4,  7,  9])
points5.append([0,  3,  6,  9, 12])
points5.append([0,  0,  3,  6,  9])
points5.append([0,  2,  2,  2,  4])
points5.append([0,  2,  3,  4,  6])
points5.append([0,  2,  4,  6,  8])
points5.append([0,  2,  5,  8, 10])
points5.append([0,  2,  4,  6,  9])
points5.append([0,  4,  6,  8, 12])
points5.append([0,  2,  4,  8,  8])
points5.append([0,  1,  1,  1,  2])
points5.append([0,  1,  2,  3,  4])
points5.append([0,  1,  3,  5,  6])
points5.append([0,  1,  4,  7,  8])
points5.append([0,  1,  5,  9, 10])
points5.append([0,  1,  3,  4,  8])
points5.append([0,  5,  6,  7, 12])
points5.append([0,  1,  2,  7,  7])
points5.append([0,  0,  0,  0,  0])
points5.append([0,  0,  1,  2,  2])
points5.append([0,  0,  2,  4,  4])
points5.append([0,  0,  3,  6,  6])
points5.append([0,  0,  4,  8,  8])
points5.append([0,  0,  5, 10, 10])
points5.append([0,  0,  6, 12, 12])
points5.append([0,  0,  0,  0,  6])
points5.append(center(5))
points[5] = points5

points6 = []
points6.append([0,  6,  6,  6,  6, 12])
points6.append([0,  0,  0,  0,  6,  6])
points6.append([0,  0,  6,  6, 12, 12])
points6.append([0,  0,  0,  0,  0,  0])
points6.append([0,  0,  0, 12, 12, 12])
points6.append([0,  0,  0,  6,  6,  6])
points[6] = points6

points7 = []
points7.append([0,  0,  0,  0,  0,  0,  0])
points7.append([0,  6,  6,  6,  6,  6, 12])
points7.append([0,  0,  6,  6,  6, 12, 12])
points7.append([0,  0,  0,  6, 12, 12, 12])
#~points7.append(center(7))
points[7] = points7

hyperplane_equations_for_dimensions = {}

def print_hyperplane_equation(dimensions):
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
            u, c = hyperplane_equation_by_cross_product(points_, True)
            hyperplane_equations_for_dimensions[dimensions]["Cross Product"] = (u, c)
            print()
        except:
            pass
        try:
            print("Using nullspace of vectors:".upper())
            u, c = hyperplane_equation_by_nullspace_from_vectors(points_, True)
            hyperplane_equations_for_dimensions[dimensions]["Nullspace of Vectors"] = (u, c)
            print()
        except:
            pass
        try:
            print("Using singular value decomposition of vectors:".upper())
            u, c = hyperplane_equation_by_svd_from_vectors(points_, True)
            hyperplane_equations_for_dimensions[dimensions]["Singular Value Decomposition"] = (u, c)
            print()
        except:
            pass
    print("Using cyclical region of OPT:".upper())
    u, c = hyperplane_equation_from_dimensonality(dimensions, True)  
    hyperplane_equations_for_dimensions[dimensions]["Cyclical Region"] = (u, c)
    print()
    
def print_hyperplane_equations():
    for dimensions in range(3, 13):
        print_hyperplane_equation(dimensions)
        
def test_hyperplane_equation(u, c, chord, is_invariant):
    reflection = reflect(chord, u, c)
    if is_invariant == True:
        print("reflection should be invariant:\n    %s\n    %s" % (chord, reflection))
    else:
        print("reflection should not be invariant:\n    %s\n    %s" % (chord, reflection))
    involution = reflect(reflection, u, c)
    print("should be an involution:\n    %s\n    %s" % (chord, involution))
    print()
    
test_chords = []
test_chords.append((center(3), True))
test_chords.append(([0, 2, 7], True))
test_chords.append(([0, 0, 6], True))
test_chords.append(([0, 1, 4], False))
test_chords.append(([0, 3, 6], False))
test_chords.append(([-1, 2, 8], False))
test_chords.append((center(4), True))
test_chords.append((center(5), True))
test_chords.append((center(6), True))
test_chords.append((center(7), True))
test_chords.append((center(8), True))
test_chords.append((center(9), True))
test_chords.append((center(7), True))
test_chords.append((center(10), True))
test_chords.append((center(11), True))
test_chords.append((center(12), True))
    
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
                test_hyperplane_equation(equation[0], equation[1], chord[0], chord[1])
            except:
                print("Exception...")
                print()
        print()
print()
print_hyperplane_equations()
print()
test_hyperplane_equations()
