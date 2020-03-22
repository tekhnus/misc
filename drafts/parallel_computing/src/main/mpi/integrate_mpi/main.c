#include <mpi.h>
#include <stdio.h>
#include <math.h>

// *** //

double integrate_with_for(double (*f)(double), double left, double right, unsigned nsteps);

/*
 * Integrates a function using MPI.
 * This must be run on ALL processes.
 * The function returns the integral on its area of responsibility.
 * The process number 0 is responsible for the whole interval.
 * Thus, the value of the whole integral is returned by process with rank 0.
 */
double integrate_with_mpi(double (*f)(double), double left, double right, unsigned nsteps);

// *** //


double integrate_with_for(double (*f)(double), double left, double right, unsigned nsteps) {
    double base = (right - left) / nsteps;
    double area = 0.0;
    unsigned step;
    for (step = 0; step < nsteps; step++) {
        // slow but precise
        double x = left + (right - left) * step / nsteps;
        area += base * f(x);
    }
    return area;
}


double integrate_with_mpi(double (*f)(double), double left, double right, unsigned nsteps) {
    
    double base = (right - left) / nsteps;
    double area = 0.0;
    
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank); 
    MPI_Comm_size(MPI_COMM_WORLD, &size);

        
    unsigned chunk_nsteps = nsteps / size + 1; // +1 just in case
    double chunk_width = (right - left) / size;
    double chunk_left = left + chunk_width * rank;
    double chunk_right = chunk_left + chunk_width;
    double chunk_area = integrate_with_for(f, chunk_left, chunk_right, chunk_nsteps);
        
    int first_child = 2*rank + 1;
    int second_child = 2*rank + 2;

    int has_first = first_child < size;
    int has_second = second_child < size;
    
    double first_area = 0.0;
    double second_area = 0.0;

    if (has_first) {
        MPI_Recv(&first_area, 1, MPI_DOUBLE, first_child, 0,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }
    if (has_second) {
        MPI_Recv(&second_area, 1, MPI_DOUBLE, second_child, 0,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }

    double all_area = chunk_area + first_area + second_area;
    
    if (rank != 0) {
        int parent = (rank - 1) / 2;
        MPI_Send(&all_area, 1, MPI_DOUBLE, parent, 0, MPI_COMM_WORLD);
    }

    return all_area;
}

// *** //

double test_function(double x) {
    return sqrt(4 - x*x);
}

// *** //

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);
    
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    double single_result = integrate_with_for(test_function, -2.0, 2.0, 30000);
    double mpi_result = integrate_with_mpi(test_function, -2.0, 2.0, 30000);
    if (rank == 0) {
        printf("Single: %f\nMPI: %f\n", single_result, mpi_result);
        printf("By the way, 2*pi is %f\n", 2*M_PI);
    }
    MPI_Finalize();
    return 0;
}

