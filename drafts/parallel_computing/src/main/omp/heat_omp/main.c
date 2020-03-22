#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <omp.h>

// *** //

double calculate_temperature_at_point(double last_u_left, double last_u, double last_u_right,
                                      double h, double tau, double c);

void calculate_temperature_iteration(double* last_u, double* new_u, unsigned n,
                                     double h, double tau, double c);

void calculate_temperature(double* u, unsigned n,
                           double length, unsigned s, double time, double c);

void fill_initial_u(double* u, unsigned n);

double get_optimal_tau(double h, double c, double k);

// *** //

double calculate_temperature_at_point(double last_u_left, double last_u, double last_u_right,
                                      double h, double tau, double c) {
    return last_u + c*tau/(h*h) * (last_u_left + last_u_right - 2*last_u);
}

void calculate_temperature_iteration(double* last_u, double* new_u, unsigned n,
                                     double h, double tau, double c) {
    new_u[0] = last_u[0];
    new_u[n - 1] = last_u[n - 1];
    unsigned j;
    #ifdef USE_STATIC
        #pragma omp for schedule(static, STATIC_CHUNK)
    #elif USE_DYNAMIC
        #pragma omp for schedule(dynamic)
    #elif USE_GUIDED
        #pragma omp for schedule(guided)
    #else
        #error "Please define schedule type with macro"
    #endif
    for (j = 1; j < n - 1; ++j) {
        new_u[j] = 
            calculate_temperature_at_point(last_u[j - 1], last_u[j], last_u[j + 1],
                                           h, tau, c);
    }
}

void calculate_temperature(double* u, unsigned n,
                           double length, unsigned s, double time, double c) {
    if (s % 2 == 1) {
        ++s; // s must be even, or final_u will be swapped with buffer
    }
    double tau = time / s;
    double h = length / n;
    fill_initial_u(u, n);
    double* buffer = calloc(n, sizeof(double));
    double* tmp;
    unsigned i;
    #pragma omp parallel shared(u, n, length, s, time, c, tau, h, buffer, tmp) private(i)
    for (i = 0; i < s; ++i) {
        calculate_temperature_iteration(u, buffer, n, h, tau, c);
        
        #pragma omp barrier
        #pragma omp single
        {
            tmp = u;
            u = buffer;
            buffer = tmp;
        }
    }
    free(buffer);
}

void fill_initial_u(double* u, unsigned n) {
    unsigned i;
    for (i = 0; i < n - 1; ++i) {
        u[i] = 0.0;
    }
    u[n - 1] = 1.0;
}

double get_optimal_tau(double h, double c, double k) {
    return k * h*h / c;
}

// *** //

const double length = 1.0;
const double c = 1.0;
const double k = 0.3;

void print_array(double* arr, size_t length) {
    fprintf(stderr, "[\n");
    size_t i;
    for (i = 0; i < length; ++i) {
        fprintf(stderr, "  %f,\n", arr[i]);
    }
    fprintf(stderr, "]\n");
}

int main(int argc, const char *argv[])
{
    if (argc < 3) {
        printf("Usage: %s n time [print]\n", argv[0]);
        exit(1);
    }
    unsigned n = atoi(argv[1]);
    double time = atof(argv[2]);
    int print = argc > 3;
    double h = length / n;
    double tau = get_optimal_tau(h, c, k);
    unsigned s = time / tau;
    double* u = calloc(n, sizeof(double));
    
    if (print) {
        fprintf(stderr, "s=%u\n", s);
    }

    double start_time = omp_get_wtime();
    calculate_temperature(u, n, length, s, time, c);
    double end_time = omp_get_wtime();

    if (print) {
        print_array(u, n);
    }
    printf("%f,\n", end_time - start_time);

    free(u);

    return 0;
}

