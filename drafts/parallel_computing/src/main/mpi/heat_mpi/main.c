#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// *** //

double calculate_temperature_at_point(double last_u_left, double last_u,
                                      double last_u_right,
                                      double h, double tau, double c);

double get_optimal_tau(double h, double c, double k);

double calculate_temperature(double* u, unsigned chunk_size, double h,
                                        unsigned s, double tau,
                             double c, int rank, int size);

// *** //

double calculate_temperature_at_point(double last_u_left, double last_u,
                                      double last_u_right,
                                      double h, double tau, double c) {
    return last_u + c*tau/(h*h) * (last_u_left + last_u_right - 2*last_u);
}

double get_optimal_tau(double h, double c, double k) {
    return k * h*h / c;
}

double request_buffer(int target_rank, int size) {
    if (target_rank < 0) {
        return 0.0;
    }
    else if (target_rank >= size) {
        return 1.0;
    }
    double recv;
    MPI_Recv(&recv, 1, MPI_DOUBLE, target_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    return recv;
}

void send_buffer(int target_rank, int size, double data) {
    if (target_rank < 0 || target_rank >= size) {
        return;
    }
    MPI_Send(&data, 1, MPI_DOUBLE, target_rank, 0, MPI_COMM_WORLD);
}

double calculate_temperature(double* u, unsigned chunk_size, double h,
                                        unsigned s, double tau,
                             double c, int rank, int size) {
    if (s % 2 == 1) {
        ++s; // only even will work properly!
    }

    double* buffer = calloc(chunk_size, sizeof(double));
    memcpy(buffer, u, chunk_size * sizeof(double));
    double* tmp;

    double most_left, most_right;
    unsigned time_iter;
    for (time_iter = 0; time_iter < s; ++time_iter) { 
        if (rank % 2 == 0) {
            most_left = request_buffer(rank - 1, size);
            most_right = request_buffer(rank + 1, size);
            send_buffer(rank + 1, size, buffer[chunk_size - 1]);
            send_buffer(rank - 1, size, buffer[0]);
        }
        else {
            send_buffer(rank + 1, size, buffer[chunk_size - 1]);
            send_buffer(rank - 1, size, buffer[0]);
            most_left = request_buffer(rank - 1, size);
            most_right = request_buffer(rank + 1, size);
        }
        
        unsigned pos;
        for (pos = 1; pos < chunk_size - 1; ++pos) {
            u[pos] =
                calculate_temperature_at_point(buffer[pos - 1], buffer[pos],
                        buffer[pos + 1], h, tau, c);
        }
        u[0] = calculate_temperature_at_point(most_left, buffer[0], buffer[1],
                                              h, tau, c);
        u[chunk_size - 1] = calculate_temperature_at_point(buffer[chunk_size - 2],
                buffer[chunk_size - 1], most_right, h, tau, c);

        tmp = u;
        u = buffer;
        buffer = tmp;
    }
    free(tmp);
}

unsigned chunck_size_of(int rank, int size, unsigned n) {
    unsigned chunck_size = n / size;
    if (rank + 1 == size) {
        chunck_size += n % size;
    }
    return chunck_size;
}

// *** //

const double length = 1.0;
const double c = 1.0;
const double k = 0.3;

void print_chunk(double* u, unsigned chunk_size, int rank, int size, unsigned n) {
    if (rank == 0) {
        size_t i;
        for (i = 0; i < chunk_size; ++i) {
            printf("%f\n", u[i]);
        }
        double* buffer = calloc(n / size + n % size, sizeof(double));
        int next_rank;
        for (next_rank = 1; next_rank < size; ++next_rank) {
            unsigned next_chunck_size = chunck_size_of(next_rank, size, n);
            MPI_Recv(buffer, next_chunck_size, MPI_DOUBLE, next_rank, 0,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            size_t i;
            for (i = 0; i < next_chunck_size; ++i) {
                printf("%f\n", buffer[i]);
            }
        }
        free(buffer);
    }
    else {
        MPI_Send(u, chunk_size, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
    }
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);
    
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    if (argc < 3) {
        if (rank == 0) {
            printf("Usage: %s n time [noprint]\n", argv[0]);
        }
        MPI_Finalize();
        exit(1);
    }

    unsigned n = atoi(argv[1]);
    
    if (n < 2) {
        if (rank == 0) {
            printf("n must be greater than 1\n");
        }
        MPI_Finalize();
        exit(1);
    }

    int max_size = n / 2;
    if (size > max_size) {
        size = max_size;
    }
    if (rank >= size) {
        MPI_Finalize();
        return 0;
    }

    double time = atof(argv[2]);
    double h = length / n;
    double tau = get_optimal_tau(h, c, k);
    unsigned s = time / tau;
    
    unsigned chunk_size = chunck_size_of(rank, size, n);

    double* u = calloc(chunk_size, sizeof(double));
    size_t i;
    for (i = 0; i < chunk_size; ++i) {
        u[i] = 0.0;
    }

    calculate_temperature(u, chunk_size, h, s, tau, c, rank, size);
    if (argc == 3) {
        print_chunk(u, chunk_size, rank, size, n);
    }

    free(u);
    MPI_Finalize();
    return 0;
}
