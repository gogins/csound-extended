'''
INVERSION FLATS
Michael Gogins
16 August 2020

Prototype code for computing unit normal vectors for inversion flats that will 
generate OPTI chords from non-eI OPT chords.

I have used several methods in order to develop my understanding of what is 
going on and how best to do this.
'''
print(__doc__)

import scipy
import scipy.linalg
import scipy.spatial
import scipy.spatial.distance
import sys

print(sys.version)

print("scipy:", scipy.__version__)

print("\nSee 'generalized cross product' from https://math.stackexchange.com/questions/2723294/how-to-determine-the-equation-of-the-hyperplane-that-contains-several-points and https://madoshakalaka.github.io/2019/03/02/generalized-cross-product-for-high-dimensions.html...\n")

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
        
def generalized_cross_product(vectors):
    dim = len(vectors[0])
    product = []
    for j in range(dim):
        basis_vector = [0] * dim
        basis_vector[j] = 1
        print("basis_vector:", basis_vector)
        matrix = scipy.vstack([vectors, basis_vector])
        print("matrix:", matrix)
        print("determinant of matrix:", scipy.linalg.det(matrix))
        product.append(scipy.linalg.det(matrix))
    return product
    
def inversion_flat_by_cross_product(points, t_equivalence = 'True'):
    vectors = []
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    print("points:", points)
    print("determinant of points:", scipy.linalg.det(points))
    subtrahend = points[-1]
    print ("subtrahend:", subtrahend)
    for i in range(len(points) - 1):
        vector = scipy.subtract(points[i], subtrahend)
        print("vector[", i, "]:", vector)
        vectors.append(vector)
    print("vectors:", vectors)
    product = generalized_cross_product(vectors) 
    print("generalized_cross_product:", product)
    norm = scipy.linalg.norm(product)
    print("norm:", norm)
    unit_normal = scipy.divide(product, norm)
    print("unit normal vector:", unit_normal)
    constant_term = scipy.dot(scipy.transpose(unit_normal), subtrahend)
    print("constant_term:", constant_term)
    return unit_normal, constant_term
    
def inversion_flat_by_nullspace(points, t_equivalence = 'True'):
    vectors = []
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    print("points:", points)
    try:
        print("determinant of points:", scipy.linalg.det(points))
    except:
        pass
    subtrahend = points[-1]
    print ("subtrahend:", subtrahend)
    for i in range(len(points) - 1):
        vector = scipy.subtract(points[i], subtrahend)
        print("vector[", i, "]:", vector)
        vectors.append(vector)
    nullspace = scipy.linalg.null_space(vectors)
    print("nullspace from vectors:", nullspace)
    homogeneous_points = []
    if True:
        for point in points:
            homogeneous_points.append(scipy.append(point, [1]))
        print("homogeneous points:", homogeneous_points)
    nullspace_from_points = scipy.linalg.null_space(homogeneous_points)
    nullspace_from_points = scipy.ndarray.flatten(nullspace_from_points[:-1])
    print("nullspace from homogeneous points:", nullspace_from_points)
    constant_term = scipy.dot(scipy.transpose(nullspace), subtrahend)
    print("constant_term:", constant_term)
    return nullspace_from_points, constant_term 
    
def inversion_flat_by_nullspacex(points, t_equivalence = 'True'):
    temp = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            temp.append( eT(point))
        points = temp
    print("points:", points)
    try:
        print("determinant of points:", scipy.linalg.det(points))
    except:
        pass
    homogeneous_points = []
    for point in points:
        homogeneous_points.append(scipy.append(point, [1]))
    print("homogeneous points:", homogeneous_points)
    nullspace = scipy.linalg.null_space(homogeneous_points)
    print("nullspace from homogeneous points:", nullspace)
    constant_term = scipy.dot(scipy.transpose(nullspace), homogeneous_points[0])
    print("constant_term:", constant_term)
    return nullspace, constant_term
    
def inversion_flat_by_svd(points, t_equivalence = 'True'):
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    temp = []
    if False:
        for point in points:
            temp.append(scipy.append(point, [1]))
        points = temp
    print("points:", points)
    svd_ = scipy.linalg.svd(points)    
    print("left singular vectors:", svd_[0])
    print("singular values:", svd_[1])
    print("right singular vectors:", svd_[2])
    minimum_singular_value = min(svd_[1])
    index_ = list(svd_[1]).index(minimum_singular_value)
    unit_normal_vector = svd_[2][index_]
    print("unit_normal_vector:", unit_normal_vector)
    constant_term = scipy.dot(scipy.transpose(unit_normal_vector), points[0])
    print("constant_term:", constant_term)
    return unit_normal_vector, constant_term       
    
def inversion_flat_by_least_squares(points, t_equivalence = 'True'):
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    if True:
        temp = []
        for point in points:
            temp.append(scipy.append(point, [1]))
        points = temp
    print("points:", points)
    zeros = [0] * len(points)
    fit = scipy.linalg.lstsq(points, zeros)
    print("solution:", fit[0])
    print("residues:", fit[1])
    print("rank:", fit[2])
    print("singular values:", fit[3])
    return fit
    
def distance_to_origin(v, u, c):
    numerator = abs(scipy.dot(v, u) + c)
    denominator = scipy.linalg.norm(u)**2
    distance = numerator / denominator
    return distance
    
    
# Ref(v,c) = v - 2 {[(v . u) - c] / (u . u)} u.
def reflect(v, u, c):
    print("Reflect by vector math:", v, " in ", u, c)
    v_dot_u = scipy.dot(v, u)
    print("v_dot_u:", v_dot_u)
    v_dot_u_minus_c = v_dot_u + c
    print("v_dot_u_minus_c:", v_dot_u_minus_c)
    u_dot_u = scipy.dot(u, u)
    print("u_dot_u:", u_dot_u)
    quotient = v_dot_u_minus_c / u_dot_u
    print("quotient:", quotient)
    subtrahend = ((2 * quotient) * u)
    print("subtrahend:", subtrahend)
    reflection = v - subtrahend
    print("reflection by vector math:", reflection)
    return reflection
    
def reflect_by_householder(v, u, c):
    print("Reflect by Householder:", v, " in ", u, c)
    tensor_ = scipy.outer(u, u);
    print("tensor_:", tensor_)
    product_ = tensor_ * 2
    print("product_:", product_)
    identity_ = scipy.eye(len(v))
    print("identity_:", identity_)
    householder = identity_ - tensor_;
    print("householder:", householder)
    translated_voices = v - c
    print("translated_voices:", translated_voices)
    reflected_translated_voices = scipy.matmul(translated_voices, householder); 
    print("reflected_translated_voices:", reflected_translated_voices)
    reflection = reflected_translated_voices + c;
    print("reflection by householder:", reflection)
    return reflection
    
    
test_points = []
test_points.append([4,0,-1,0])
test_points.append([1,2,3,-1])
test_points.append([ 0,-1,2,0])
test_points.append([-1,1,-1,1])

print("\nTest points by generalized cross product\n".upper())
print("Generalized cross product should be: [13, 8, 20, 57].")
inversion_flat_by_cross_product(test_points, False)
print("\nTest points by null space\n".upper())
inversion_flat_by_nullspace(test_points, False)
print("\nTest points by SVD\n".upper())
inversion_flat_by_svd(test_points, False)
#~ print("\nTest points by least squares\n".upper())
#~ inversion_flat_by_least_squares(test_points, False)

print("\nInversion flats from _Science_...\n".upper())

print('''
SAMPLE OPT CHORDS FOR 4 VOICES

OPT:   15 [  -6.0000000   -4.0000000    4.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    2.0000000   10.0000000 ] iseI: true
OPT:   16 [  -6.0000000   -3.0000000    3.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    3.0000000    9.0000000 ] iseI: true
OPT:   17 [  -6.0000000   -3.0000000    4.0000000    5.0000000 ] sum:    0.0000 span:   11.0000 chord type: [   0.0000000    1.0000000    4.0000000   11.0000000 ] iseI: false
OPT:   18 [  -6.0000000   -2.0000000    2.0000000    6.0000000 ] sum:    0.0000 span:   12.0000 chord type: [   0.0000000    0.0000000    4.0000000    8.0000000 ] iseI: true
OPT:   19 [  -6.0000000   -2.0000000    3.0000000    5.0000000 ] sum:    0.0000 span:   11.0000 chord type: [   0.0000000    1.0000000    5.0000000   10.0000000 ] iseI: false
''')

points = []
points.append([0,0,0,0])
points.append([0,3,6,9])
points.append([0,0,0,12])
points.append([0,1,2,9])

print("\n4 voices by cross product...\n".upper())
u, c = inversion_flat_by_cross_product(points[:4], True)

normal = scipy.subtract(eT([0, 0,0,6]), eT([0, 0, 6,6]))
normal = scipy.subtract(eT([0, 0,3,6]), eT([0, 3, 6,9]))
print("\nnormal:", normal)
norm_ = scipy.linalg.norm(normal)
print("norm:", norm_)
unit_normal = normal / norm_
print("unit_normal:", unit_normal)
constant_term = scipy.dot(unit_normal, eT(points[0])) / norm_
print("constant_term:", constant_term)
u = unit_normal
c = constant_term
distance = distance_to_origin([0, 1,2,3], u, c)
print("\ndistance to origin:", distance)
print("type:", type(c))
#c = 2.

print("\n4 voices by nullspace...\n".upper())
ux, cx = inversion_flat_by_nullspace(points, True)
print("\n4 voices by nullspacex...\n".upper())
inversion_flat_by_nullspacex(points, True)
print("\n4 voices by SVD...\n".upper())
ux, cx = inversion_flat_by_svd(points, True)

chord = eT([0,2,4,8])
print("\nChord on inversion flat:", chord)
reflection = reflect(chord, u, c)
print("Its reflection should be invariant:", chord, reflection)
reflection = reflect(reflection, u, scipy.float64(distance))
print("Better be an involution:", reflection)
reflection = reflect_by_householder(chord, u, scipy.float64(0))
print("Its reflection should be invariant:", chord, reflection)
reflection = reflect_by_householder(reflection, u, scipy.float64(0))

print("Better be an involution:", reflection)
chord = eT([0,1,2,7])
print("\nChord on inversion flat:", chord)
reflection = reflect(chord, u, c)
print("Its reflection should be invariant:", chord, reflection)
reflection = reflect(reflection, u, scipy.float64(distance))
print("Better be an involution:", reflection)
reflection = reflect_by_householder(chord, u, scipy.float64(0))
print("Its reflection should be invariant:", chord, reflection)
reflection = reflect_by_householder(reflection, u, scipy.float64(0))
print("Better be an involution:", reflection)

non_ei_chord = eT([-6.0000000,   -2.0000000,    3.0000000,   5.0000000])
non_flat_chord = eT([0,0,3,5])
print("\nChord not on inversion flat:", non_flat_chord)
reflection = reflect(non_flat_chord, u, c)
reflection = reflect(reflection, u, c)
print("Better be an involution:", reflection)
reflection = reflect_by_householder(non_flat_chord, u, scipy.float64(0))
print("Its reflection should be eI:", reflection)
reflection = reflect_by_householder(reflection, u, scipy.float64(0))
print("Better be an involution:", reflection)


