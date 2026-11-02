/*
 * C API of libCrysP.
 *
 * Mirrors the bind(C) types and entry points in src/c_binding/{utils,micro,meso,macro,libcrysp}_c.f90. The structs must match
 * their Fortran counterparts field for field: keep both in sync when changing either.
 *
 * Ownership
 * ---------
 * Every pointer in these structs is owned, except CryspGrain::phase. Owned pointers are allocated with malloc: libCrysP does so
 * for everything it builds, and a caller building these structs must do the same. crysp_destroy_<type> releases everything a
 * struct owns, with libc's free(). It does not free the struct itself, which may live on the stack or inside a list, and it
 * assumes the struct is well-formed: afterwards its pointers dangle, so it must not be used or destroyed again. Lists returned by
 * libCrysP are released by destroying each element and then calling free() on the list.
 *
 * Arrays
 * ------
 * Matrices are stored column-major, as in Fortran: element (i,j) of a 3x3 matrix m is m[j][i], and pointer-backed matrices hold
 * their columns one after the other.
 */
#ifndef LIBCRYSP_H
#define LIBCRYSP_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Buffer sizes of the fixed-length strings, including the NUL terminator. */
#define CRYSP_NAME_LEN 56
#define CRYSP_DESC_LEN 1024
#define CRYSP_MSG_LEN  128

/* Status codes. */
enum {
    CRYSP_OK       = 0,  /* Successful execution. */
    CRYSP_ERR      = -1, /* General error. */
    CRYSP_ERR_DIMS = -2, /* Out of bounds. */
    CRYSP_ERR_VAL  = -3, /* Unacceptable value. */
    CRYSP_ERR_IO   = -4, /* Error during an IO operation. */
    CRYSP_ERR_INIT = -5, /* Procedure call without proper initialization. */
    CRYSP_ERR_TYPE = -6, /* Erroneous type provided. */
    CRYSP_ERR_ARG  = -7  /* Invalid argument provided by caller. */
};

/* Element types of a CryspParameter. */
enum {
    CRYSP_TYPE_INTEGER = 0, /* int */
    CRYSP_TYPE_REAL    = 1, /* double */
    CRYSP_TYPE_STRING  = 2  /* char */
};

/* Kinds of user input, see CryspInput. */
enum {
    CRYSP_INPUT_REAL        = 0, /* CRYSP_TYPE_REAL, rank 0. */
    CRYSP_INPUT_ANGLES_LIST = 1  /* CRYSP_TYPE_REAL, rank 2, shape (2,n). */
};

/*
 * Outcome of an entry point, returned by value. code is CRYSP_OK on success. The strings are NUL-terminated and empty on success.
 */
typedef struct {
    int  code;
    char mod_name[CRYSP_NAME_LEN];  /* Module in which the failure was raised. */
    char proc_name[CRYSP_NAME_LEN]; /* Routine in which the failure was raised. */
    char message[CRYSP_MSG_LEN];    /* Human-readable description of the failure. */
} CryspStatus;

/*
 * A value of one of the CRYSP_TYPE_* element types. data holds extents[0]*extents[1] elements. Scalars have extents
 * (1,1), arrays (n,1) and matrices (m,n). A string has rank 0 and is extents[0] characters; strings from libCrysP are also
 * NUL-terminated, which extents does not count. A parameter that was never set has rank -1 and a null data.
 */
typedef struct {
    int   type;
    int   rank;
    int   extents[2];
    void *data;
} CryspParameter;

/* An input a model requests from the user. Unset bounds and default value have rank -1. */
typedef struct {
    char           name[CRYSP_NAME_LEN];
    char           description[CRYSP_DESC_LEN];
    int            type;                   /* One of CRYSP_INPUT_*. */
    CryspParameter lower_bound;            /* A value, or the name of another input. */
    bool           lower_bound_inclusive;
    CryspParameter upper_bound;            /* A value, or the name of another input. */
    bool           upper_bound_inclusive;
    CryspParameter default_value;
} CryspInput;

/* A piece of state a model keeps. */
typedef struct {
    char name[CRYSP_NAME_LEN];
    char description[CRYSP_DESC_LEN];
    int  type;                         /* One of CRYSP_TYPE_*. */
    int  rank;
} CryspStateDescriptor;

/* Description of a model: its inputs and the state it keeps per model and per element. */
typedef struct {
    int                   id;
    char                  name[CRYSP_NAME_LEN];
    char                  description[CRYSP_DESC_LEN];
    int                   n_inputs;
    CryspInput           *inputs;          /* List of n_inputs. */
    int                   n_model_state;
    CryspStateDescriptor *model_state;     /* List of n_model_state. */
    int                   n_element_state;
    CryspStateDescriptor *element_state;   /* List of n_element_state. */
} CryspModelDescriptor;

/* Description of a set of slip systems. */
typedef struct {
    int  id;
    char name[CRYSP_NAME_LEN];
    char description[CRYSP_DESC_LEN];
} CryspSlipSystemDescriptor;

/* A model ID with its parameters: the inputs of a new model, or the state of an existing one. */
typedef struct {
    int             id;
    int             n_parameters;
    CryspParameter *parameters;   /* List of n_parameters. */
} CryspModelData;

/* Everything needed to create a phase. */
typedef struct {
    CryspModelData model;                 /* Hardening model and its inputs. */
    int            deformation_mechanism; /* ID of a slip system set, see crysp_get_slip_systems(). */
    int            n_orientations;
    double        *orientations;          /* 3 x n_orientations: Euler angles. */
} CryspPhaseData;

/* The constitutive model of a phase. */
typedef struct {
    CryspModelData hardening_model_state; /* Model ID and model-specific state. */
    int            n_systems;
    double        *taylor_coeffs;         /* 5 x n_systems. */
    double        *spin_coeffs;           /* 3 x n_systems. */
    int            basis[5];
} CryspConstitutiveModel;

/* Hardening state of a grain. */
typedef struct {
    int             n_systems;
    double         *crss;       /* 2 x n_systems. */
    int             n_params;
    CryspParameter *params;     /* List of n_params: state specific to the hardening model. */
} CryspHardeningState;

/* A grain. */
typedef struct {
    CryspConstitutiveModel *phase;            /* The element of the material's phases backing this grain. */
    double                  orientation[3][3];
    double                  stress_increment;
    CryspHardeningState     hardening_state;
} CryspGrain;

/* A cluster of grains. */
typedef struct {
    int             n_grains;
    CryspGrain     *grains;   /* List of n_grains. */
    double          weight;
    int             n_state;
    CryspParameter *state;    /* List of n_state: state specific to the meso model. */
} CryspCluster;

/* A material. The phase of every grain in clusters points to an element of phases. */
typedef struct {
    CryspModelData          meso_model;  /* Meso model ID and model-specific state. */
    int                     n_phases;
    CryspConstitutiveModel *phases;      /* List of n_phases. */
    int                     n_clusters;
    CryspCluster           *clusters;    /* List of n_clusters. */
} CryspMaterial;

/* One increment of a deformation. */
typedef struct {
    double duration;
    double deformation_gradient[3][3];
    double vm_strain;
    double stress[3][3];
    double taylor_factor;
    int    n_grains;
    int   *texture;                    /* 3 x n_grains: Euler angles in degrees. */
} CryspStrainIncrement;

/* One increment of a stress-driven deformation, made up of strain increments. */
typedef struct {
    double                strain_rate[5];
    double                residual[5];
    int                   n_strain_increments;
    CryspStrainIncrement *strain_increments;  /* List of n_strain_increments. */
} CryspStressIncrement;

/*
 * Describe the available slip system sets as a list of *n_systems descriptors, including their IDs. The descriptors own nothing,
 * so the list is released with free() alone.
 */
void crysp_get_slip_systems(int *n_systems, CryspSlipSystemDescriptor **descs);

/* Describe the available meso or micro (hardening) models as a list of *n_models descriptors, including their IDs. */
void crysp_get_meso_models(int *n_models, CryspModelDescriptor **model_descs);
void crysp_get_micro_models(int *n_models, CryspModelDescriptor **model_descs);

/* Create a material from a meso model and n_phases phases. On failure, *mat is undefined and must not be destroyed. */
CryspStatus crysp_new_material(const CryspModelData *meso_model_data, int n_phases, const CryspPhaseData *phase_data, CryspMaterial *mat);

/*
 * Deform *mat under a velocity gradient until the von Mises strain reaches target_vm_strain, or along a stress mode.
 * velocity_gradient is not modified; it is not declared const because C before C23 rejects passing a double[3][3] to a const one.
 *
 * *mat is replaced by the deformed material, and what it owned on entry is destroyed. This also happens on failure, where *mat
 * holds the state reached when the failure occurred. *incs is a list of *n_incs increments, empty (NULL) on failure.
 */
CryspStatus crysp_strain_driven_deformation(CryspMaterial *mat, double velocity_gradient[3][3], double target_vm_strain, int *n_incs, CryspStrainIncrement **incs);
CryspStatus crysp_stress_driven_deformation(CryspMaterial *mat, const double target_stress_mode[5], double target_vm_strain,
                                            int *n_incs, CryspStressIncrement **incs);

/*
 * Find the strain mode yielding a stress in the direction of target_stress_mode (unit norm), starting from the guess in
 * strain_mode, or compute the stress for a given strain mode. *mat is not modified.
 */
CryspStatus crysp_simulate_stress_mode(const CryspMaterial *mat, const double target_stress_mode[5], double strain_mode[5], double stress[5], double residual[5]);
CryspStatus crysp_simulate_strain_mode(const CryspMaterial *mat, const double strain_mode[5], double stress[5]);

/* Release everything a struct owns, see "Ownership" above. */
void crysp_destroy_parameter(CryspParameter *param);
void crysp_destroy_input(CryspInput *input);
void crysp_destroy_model_descriptor(CryspModelDescriptor *desc);
void crysp_destroy_model_data(CryspModelData *model_data);
void crysp_destroy_phase_data(CryspPhaseData *phase_data);
void crysp_destroy_constitutive_model(CryspConstitutiveModel *model);
void crysp_destroy_hardening_state(CryspHardeningState *hardening_state);
void crysp_destroy_grain(CryspGrain *grain);
void crysp_destroy_cluster(CryspCluster *cluster);
void crysp_destroy_material(CryspMaterial *mat);
void crysp_destroy_strain_increment(CryspStrainIncrement *inc);
void crysp_destroy_stress_increment(CryspStressIncrement *inc);

#ifdef __cplusplus
}
#endif

#endif
