'''
INVERSION FLATS
Michael Gogins
16 August 2020

Prototype code for computing unit normal vectors for inversion flats that will 
generate OPTI chords from non-eI OPT chords.
'''
print(__doc__)

import scipy
import scipy.linalg
import sys

print(sys.version)

print("scipy:", scipy.__version__)

print("\nTrying 'generalized cross product' from https://math.stackexchange.com/questions/2723294/how-to-determine-the-equation-of-the-hyperplane-that-contains-several-points and https://madoshakalaka.github.io/2019/03/02/generalized-cross-product-for-high-dimensions.html...\n")

def generalized_cross_product(vectors):
    dim = len(vectors[0])
    product = []
    for j in range(dim):
        basis_vector = [0] * dim
        basis_vector[j] = 1
        print("basis_vector:", basis_vector)
        matrix = scipy.vstack([vectors, basis_vector])
        print("Matrix:", matrix)
        product.append(scipy.linalg.det(matrix))
    return product

points = []
points.append([4,0,-1,0])
points.append([1,2,3,-1])
points.append([0,-1,2,0])
points.append([-1,1,-1,1])

vectors = []
subtrahend = points[-1]
print ("subtrahend:", subtrahend)
for i in range(len(points) - 1):
    vector = scipy.subtract(points[i], subtrahend)
    print("vector[", i, "]:", vector)
    vectors.append(vector)
print("points:", points)
print("vectors:", vectors)
product = generalized_cross_product(vectors) 
print("Should be: [13, 8, 20, 57].")
print("generalized_cross_product:", product)
print("unit normal vector:", scipy.divide(product, scipy.linalg.norm(product)))

print("\nTrying null space...\n")

nullspace = scipy.linalg.null_space(vectors)
print("nullspace:", nullspace)
print("\nNote, the null space gives the unit normal vector directly.\n")

print("\nNow trying with inversion flats from _Science_...\n")

points = []
vectors = []
points.append([ 0, 0,   0,  6  ])
points.append([ 0, 0,   6,  6  ])
points.append([ 0, 6,   6, 12  ])
# Put this in by hand to get linear independence.
points.append([ 0, 1.5, 3,  1.5])
subtrahend = points[-1]
print ("subtrahend:", subtrahend)
for i in range(len(points) - 1):
    vector = scipy.subtract(points[i], subtrahend)
    print("vector[", i, "]:", vector)
    vectors.append(vector)
print("points:", points)
print("vectors:", vectors)
nullspace = scipy.linalg.null_space(vectors)
print("nullspace:", nullspace)
product = generalized_cross_product(vectors) 
print("generalized_cross_product:", product)
print("unit normal vector:", scipy.divide(product, scipy.linalg.norm(product)))

print("\nNow trying with inversion flats from _Science_...\n")

points = []
vectors = []
points.append([-1.5, -1.5, -1.5, 4.5 ])
points.append([-3,   -3,    3,   3   ])
points.append([ 0,    1.5,  3,   4.5 ])
# Put this in by hand to get linear independence.
points.append([-1.5,  0,	1.5, 0   ])
subtrahend = points[-1]
print ("subtrahend:", subtrahend)
for i in range(len(points) - 1):
    vector = scipy.subtract(points[i], subtrahend)
    print("vector[", i, "]:", vector)
    vectors.append(vector)
print("points:", points)
print("vectors:", vectors)
nullspace = scipy.linalg.null_space(vectors)
print("nullspace:", nullspace)
product = generalized_cross_product(vectors) 
print("generalized_cross_product:", product)
print("unit normal vector:", scipy.divide(product, scipy.linalg.norm(product)))

print("\nTrying with T equivalence...\n");
points = []
vectors = []
points.append([-1.5, -1.5, -1.5, 4.5])
points.append([-3,   -3,    3,   3  ])
points.append([ 0,    1.5,  3,   4.5 ])
# Put this in by hand to get linear independence.
points.append([-1.5,  0,	1.5, 0])
subtrahend = points[-1]
print ("subtrahend:", subtrahend)
for i in range(len(points) - 1):
    vector = scipy.subtract(points[i], subtrahend)
    print("vector[", i, "]:", vector)
    vectors.append(vector)
print("points:", points)
print("vectors:", vectors)
nullspace = scipy.linalg.null_space(vectors)
print("nullspace:", nullspace)

