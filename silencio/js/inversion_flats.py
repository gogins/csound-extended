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
    unit_normal = scipy.divide(product, scipy.linalg.norm(product))
    print("unit normal vector:", unit_normal)
    nullspace = scipy.linalg.null_space(vectors)
    print("nullspace:", nullspace)
    return unit_normal
    
def inversion_flat_by_nullspace(points, t_equivalence = 'True'):
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
    nullspace = scipy.linalg.null_space(vectors)
    print("nullspace from vectors:", nullspace)
    homogeneous_points = []
    if True:
        for point in points:
            homogeneous_points.append(scipy.append(point, [1]))
        print("homogeneous points:", homogeneous_points)
    nullspace_from_points = scipy.linalg.null_space(homogeneous_points)
    print("nullspace from homogeneous points:", nullspace)
    return nullspace_from_points
    
def inversion_flat_by_svd(points, t_equivalence = 'True'):
    vectors = []
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    temp = []
    for point in points:
        temp.append(scipy.append(point, [1]))
    points = temp
    print("points:", points)
    # subtract out the centroid?
    if False:
        points = scipy.transpose(scipy.transpose(points) - scipy.sum(points,1) / len(points)) 
        print("centroid subtracted:", points)
    # singular value decomposition
    svd_ = scipy.linalg.svd(points)    
    print("svd:")
    print("left singular vectors:", svd_[0])
    print("singular values:", svd_[1])
    print("right singular vectors:", svd_[2])
    
def inversion_flat_by_least_squares(points, t_equivalence = 'True'):
    vectors = []
    t_ = []
    if t_equivalence == True:
        print("original points:", points)
        for point in points:
            t_.append( eT(point))
        points = t_
    if False:
        temp = []
        for point in points:
            temp.append(scipy.append(point, [1]))
        points = temp
        print("points:", points)
    #~ vectors = []
    #~ subtrahend = points[-1]
    #~ print ("subtrahend:", subtrahend)
    #~ for i in range(len(points) - 1):
        #~ vector = scipy.subtract(points[i], subtrahend)
        #~ print("vector[", i, "]:", vector)
        #~ vectors.append(vector)
    zeros = [0] * len(points)
    fit = scipy.linalg.lstsq(points, zeros)
    print("solution:", fit[0])
    print("residues:", fit[1])
    print("rank:", fit[2])
    print("singular values:", fit[3])
    return fit
    
def reflect(chord, unit_normal, distance):
    reflection = [0] * len(chord)
    
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
print("\nTest points by least squares\n".upper())
inversion_flat_by_least_squares(test_points, False)

print("\nInversion flats from _Science_...\n".upper())

points = []
points.append([ 0, 0,   0,  6  ])
points.append([ 0, 0,   6,  6  ])
points.append([ 0, 6,   6, 12  ])
points.append([0,2,3,5])
points.append([0,2,5,7])
points.append([10,1,0,3])
points.append([0,1,5,6])
points.append([0,1,5,8])

print("\n4 voices by cross product...\n".upper())
inversion_flat_by_cross_product(points[:4])

print("\n4 voices by nullspace...\n".upper())
inversion_flat_by_nullspace(points)

print("\n4 voices by SVD...\n".upper())
inversion_flat_by_svd(points)

print("\n4 voices by least squares...\n.upper()")
inversion_flat_by_least_squares(points, True)


