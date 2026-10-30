/*
 * Unit test of the C API, written against libcrysp.h as a C caller would use it.
 *
 * Tests the binding only, not the models behind it: the struct layout (checked against the sizes the library reports, see
 * c_binding_sizes.f90), values crossing the boundary in both directions, status reporting, the documented effect of each entry
 * point on its arguments, and ownership: a material the caller built with malloc is accepted and released by libCrysP like one
 * libCrysP built itself. Memory errors in the destroy procedures only show up when run under a sanitizer or valgrind.
 *
 * Prints "Ran <n> checks, <m> failed." like the Fortran tests and exits non-zero if any check failed.
 */
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libcrysp.h"

#define PI 3.14159265358979323846
#define N_GRAINS 4 /* Per phase. */
#define TARGET_VM_STRAIN 0.02

/* Implemented in c_binding_sizes.f90. */
void crysp_test_sizes(size_t sizes[14]);

static int n_checks = 0;
static int n_failed = 0;

static int check(int condition, const char *fmt, ...) {
    n_checks++;
    if (!condition) {
        va_list args;
        n_failed++;
        printf("FAILED: ");
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
        printf("\n");
    }
    return condition;
}

static int check_ok(CryspStatus stat, const char *what) {
    return check(stat.code == CRYSP_OK, "%s: %d %s::%s: %s", what, stat.code, stat.mod_name, stat.proc_name, stat.message);
}

static int near(double a, double b, double tol) {
    return fabs(a - b) <= tol;
}

static void unit(const double v[5], double u[5]) {
    double length = 0.;
    for (int i = 0; i < 5; i++) length += v[i] * v[i];
    length = sqrt(length);
    for (int i = 0; i < 5; i++) u[i] = v[i] / length;
}

/* Whether a fixed-size buffer holds a NUL-terminated string. */
static int terminated(const char *buffer, size_t size) {
    return memchr(buffer, '\0', size) != NULL;
}

/* ---- Model IDs, looked up by name as a C caller would. ---- */

static int alamel = -1, voce = -1, dsh_edge = -1;

static int find_model(CryspStatus (*get)(int *, CryspModelDescriptor **), const char *name) {
    int n = 0, id = -1;
    CryspModelDescriptor *descs = NULL;

    if (!check_ok(get(&n, &descs), "listing models")) return -1;
    for (int i = 0; i < n; i++) {
        if (strcmp(descs[i].name, name) == 0) id = descs[i].id;
        crysp_destroy_model_descriptor(&descs[i]);
    }
    free(descs);
    check(id >= 0, "model '%s' is described", name);
    return id;
}

static int find_models(void) {
    alamel = find_model(crysp_get_meso_models, "ALAMEL");
    voce = find_model(crysp_get_micro_models, "Voce");
    dsh_edge = find_model(crysp_get_micro_models, "Dislocation Substructural Hardening (edge variant)");
    return alamel >= 0 && voce >= 0 && dsh_edge >= 0;
}

/* ---- Caller-built copies, allocated with malloc as the ownership convention requires. ---- */

static void *copy_buffer(const void *src, size_t size) {
    void *dest = malloc(size ? size : 1);
    if (size) memcpy(dest, src, size);
    return dest;
}

static CryspParameter copy_parameter(const CryspParameter *src) {
    CryspParameter dest = *src;
    size_t n = (size_t)src->extents[0] * (size_t)src->extents[1];
    if (src->rank < 0) return dest;
    switch (src->type) {
        case CRYSP_TYPE_INTEGER: dest.data = copy_buffer(src->data, n * sizeof(int)); break;
        case CRYSP_TYPE_REAL:    dest.data = copy_buffer(src->data, n * sizeof(double)); break;
        case CRYSP_TYPE_STRING:  dest.data = copy_buffer(src->data, (size_t)src->extents[0]); break;
    }
    return dest;
}

static CryspParameter *copy_parameters(int n, const CryspParameter *src) {
    CryspParameter *dest = malloc(n ? (size_t)n * sizeof *dest : 1);
    for (int i = 0; i < n; i++) dest[i] = copy_parameter(&src[i]);
    return dest;
}

static CryspModelData copy_model_data(const CryspModelData *src) {
    CryspModelData dest = *src;
    dest.parameters = copy_parameters(src->n_parameters, src->parameters);
    return dest;
}

static CryspHardeningState copy_hardening_state(const CryspHardeningState *src) {
    CryspHardeningState dest = *src;
    dest.crss = copy_buffer(src->crss, 2 * (size_t)src->n_systems * sizeof(double));
    dest.params = copy_parameters(src->n_params, src->params);
    return dest;
}

/* The grains' phases are remapped from src_phases to dest_phases. */
static CryspCluster copy_cluster(const CryspCluster *src, const CryspConstitutiveModel *src_phases,
                                 CryspConstitutiveModel *dest_phases) {
    CryspCluster dest = *src;
    dest.grains = malloc(src->n_grains ? (size_t)src->n_grains * sizeof *dest.grains : 1);
    for (int i = 0; i < src->n_grains; i++) {
        dest.grains[i] = src->grains[i];
        dest.grains[i].phase = dest_phases + (src->grains[i].phase - src_phases);
        dest.grains[i].hardening_state = copy_hardening_state(&src->grains[i].hardening_state);
    }
    dest.state = copy_parameters(src->n_state, src->state);
    return dest;
}

static CryspMaterial copy_material(const CryspMaterial *src) {
    CryspMaterial dest = *src;
    dest.meso_model = copy_model_data(&src->meso_model);
    dest.phases = malloc((size_t)src->n_phases * sizeof *dest.phases);
    for (int i = 0; i < src->n_phases; i++) {
        const CryspConstitutiveModel *phase = &src->phases[i];
        dest.phases[i] = *phase;
        dest.phases[i].hardening_model_state = copy_model_data(&phase->hardening_model_state);
        dest.phases[i].taylor_coeffs = copy_buffer(phase->taylor_coeffs, 5 * (size_t)phase->n_systems * sizeof(double));
        dest.phases[i].spin_coeffs = copy_buffer(phase->spin_coeffs, 3 * (size_t)phase->n_systems * sizeof(double));
    }
    dest.clusters = malloc((size_t)src->n_clusters * sizeof *dest.clusters);
    for (int i = 0; i < src->n_clusters; i++) dest.clusters[i] = copy_cluster(&src->clusters[i], src->phases, dest.phases);
    return dest;
}

/* ---- Material creation from inputs on the stack. ---- */

static const double VOCE[5] = {12.39, 15., 20., 0.2, 0.1};
static const double DSH[16] = {2.48e-10, 8.16e4, 0.20, 0.20, 53.0, 2.12e-2, 8.89e-10, 8.37e-1,
                               2.66e-8, 2.27e-9, 9.59, 1.07, 5.45e-2, 1.44e-9, 4.12e-9, 8.70e-9};

typedef struct {
    double         voce[5], dsh[16];
    CryspParameter voce_params[5], dsh_params[16];
    double         orientations[2][N_GRAINS][3];
    CryspPhaseData phases[2];
    double         normals[2][2];
    CryspParameter meso_param;
    CryspModelData meso;
} MaterialInputs;

/* Two phases (Voce on FCC, DSH edge on BCC24) of N_GRAINS grains each. */
static void make_inputs(MaterialInputs *in, int meso_model_id, int hardening_model_id) {
    memcpy(in->voce, VOCE, sizeof VOCE);
    memcpy(in->dsh, DSH, sizeof DSH);
    for (int i = 0; i < 5; i++) in->voce_params[i] = (CryspParameter){CRYSP_TYPE_REAL, 0, {1, 1}, &in->voce[i]};
    for (int i = 0; i < 16; i++) in->dsh_params[i] = (CryspParameter){CRYSP_TYPE_REAL, 0, {1, 1}, &in->dsh[i]};
    for (int p = 0; p < 2; p++)
        for (int g = 0; g < N_GRAINS; g++) {
            int k = p * N_GRAINS + g + 1;
            in->orientations[p][g][0] = (97 * k % 360) * PI / 180;
            in->orientations[p][g][1] = (1 + 53 * k % 179) * PI / 180;
            in->orientations[p][g][2] = (131 * k % 360) * PI / 180;
        }
    in->phases[0] = (CryspPhaseData){{hardening_model_id, 5, in->voce_params}, CRYSP_SLIP_SYSTEMS_FCC, N_GRAINS,
                                     &in->orientations[0][0][0]};
    in->phases[1] = (CryspPhaseData){{dsh_edge, 16, in->dsh_params}, CRYSP_SLIP_SYSTEMS_BCC24, N_GRAINS,
                                     &in->orientations[1][0][0]};
    /* Grain boundary normals as (theta, phi) columns. */
    in->normals[0][0] = PI / 2; in->normals[0][1] = 0.;
    in->normals[1][0] = PI / 4; in->normals[1][1] = PI / 2;
    in->meso_param = (CryspParameter){CRYSP_TYPE_REAL, 2, {2, 2}, &in->normals[0][0]};
    in->meso = (CryspModelData){meso_model_id, meso_model_id == alamel ? 1 : 0, &in->meso_param};
}

static CryspStatus new_material(int meso_model_id, CryspMaterial *mat) {
    MaterialInputs in;
    make_inputs(&in, meso_model_id, voce);
    return crysp_new_material(&in.meso, 2, in.phases, mat);
}

/* ---- Tests. ---- */

static void test_layout(void) {
    static const char *FORTRAN_TYPES[14] = {"Status_C", "Parameter_C", "Input_C", "StateDescriptor_C", "ModelDescriptor_C",
                                            "ModelData_C", "PhaseData_C", "ConstitutiveModel_C", "HardeningState_C", "Grain_C",
                                            "Cluster_C", "Material_C", "StrainIncrement_C", "StressIncrement_C"};
    const size_t c_sizes[14] = {sizeof(CryspStatus), sizeof(CryspParameter), sizeof(CryspInput), sizeof(CryspStateDescriptor),
                                sizeof(CryspModelDescriptor), sizeof(CryspModelData), sizeof(CryspPhaseData),
                                sizeof(CryspConstitutiveModel), sizeof(CryspHardeningState), sizeof(CryspGrain),
                                sizeof(CryspCluster), sizeof(CryspMaterial), sizeof(CryspStrainIncrement),
                                sizeof(CryspStressIncrement)};
    size_t fortran_sizes[14];

    crysp_test_sizes(fortran_sizes);
    for (int i = 0; i < 14; i++)
        check(c_sizes[i] == fortran_sizes[i], "layout: size of %s is %zu in libcrysp.h but %zu in the library", FORTRAN_TYPES[i],
              c_sizes[i], fortran_sizes[i]);
}

static void test_model_descriptors(void) {
    int n = -1;
    CryspModelDescriptor *descs = NULL;

    if (check_ok(crysp_get_meso_models(&n, &descs), "crysp_get_meso_models")) {
        for (int i = 0; i < n; i++) {
            check(terminated(descs[i].name, CRYSP_NAME_LEN) && terminated(descs[i].description, CRYSP_DESC_LEN),
                  "meso model %d: strings are NUL-terminated", descs[i].id);
            crysp_destroy_model_descriptor(&descs[i]);
        }
        free(descs);
    }

    if (check_ok(crysp_get_micro_models(&n, &descs), "crysp_get_micro_models")) {
        for (int i = 0; i < n; i++) {
            CryspModelDescriptor *d = &descs[i];
            for (int j = 0; j < d->n_inputs; j++)
                check(terminated(d->inputs[j].name, CRYSP_NAME_LEN) && terminated(d->inputs[j].description, CRYSP_DESC_LEN),
                      "micro model %d, input %d: strings are NUL-terminated", d->id, j);
            if (d->id != voce || !check(d->n_inputs == 5, "Voce: expected 5 inputs, got %d", d->n_inputs)) continue;

            /* One input of each kind of parameter: a real bound, a string bound and an unset default. */
            CryspInput *crss = &d->inputs[0];
            check(strcmp(crss->name, "Initial CRSS") == 0, "Voce: first input is 'Initial CRSS', got '%s'", crss->name);
            check(crss->lower_bound.type == CRYSP_TYPE_REAL && crss->lower_bound.rank == 0 &&
                      *(double *)crss->lower_bound.data == 0.,
                  "Voce: 'Initial CRSS' has lower bound 0");
            check(crss->upper_bound.type == CRYSP_TYPE_STRING && crss->upper_bound.extents[0] == 5 &&
                      strcmp(crss->upper_bound.data, "TIIIS") == 0,
                  "Voce: 'Initial CRSS' is bounded above by the NUL-terminated input name 'TIIIS'");
            check(crss->default_value.rank == -1 && crss->default_value.data == NULL, "Voce: 'Initial CRSS' has no default");
            check(d->inputs[3].description[0] == '\0', "Voce: an input without a description gets an empty one");
        }
        for (int i = 0; i < n; i++) crysp_destroy_model_descriptor(&descs[i]);
        free(descs);
    }
}

static void test_new_material(void) {
    MaterialInputs in;
    CryspMaterial mat;

    make_inputs(&in, alamel, voce);
    CryspStatus stat = crysp_new_material(&in.meso, 2, in.phases, &mat);
    if (!check_ok(stat, "crysp_new_material")) return;
    check(stat.mod_name[0] == '\0' && stat.proc_name[0] == '\0' && stat.message[0] == '\0', "success: status strings are empty");

    check(memcmp(in.voce, VOCE, sizeof VOCE) == 0 && in.voce_params[0].data == &in.voce[0] && in.phases[0].model.id ==
              voce && in.phases[0].orientations == &in.orientations[0][0][0],
          "crysp_new_material leaves its inputs untouched");

    check(mat.meso_model.id == alamel, "material: meso model ID is %d, got %d", alamel, mat.meso_model.id);
    if (check(mat.n_phases == 2, "material: expected 2 phases, got %d", mat.n_phases))
        check(mat.phases[0].hardening_model_state.id == voce && mat.phases[1].hardening_model_state.id == dsh_edge,
              "material: phases have the requested hardening model IDs");

    int n_grains = 0, bad_phase = 0, bad_systems = 0;
    for (int c = 0; c < mat.n_clusters; c++)
        for (int g = 0; g < mat.clusters[c].n_grains; g++, n_grains++) {
            CryspGrain *grain = &mat.clusters[c].grains[g];
            if (grain->phase < mat.phases || grain->phase >= mat.phases + mat.n_phases) bad_phase++;
            else if (grain->hardening_state.n_systems != grain->phase->n_systems) bad_systems++;
        }
    check(n_grains == 2 * N_GRAINS, "material: expected %d grains, got %d", 2 * N_GRAINS, n_grains);
    check(bad_phase == 0, "material: every grain's phase points into the material's phases (%d do not)", bad_phase);
    check(bad_systems == 0, "material: every hardening state has as many slip systems as its phase");
    crysp_destroy_material(&mat);
}

static void test_errors(void) {
    MaterialInputs in;
    CryspMaterial mat;

    make_inputs(&in, 99, voce);
    CryspStatus stat = crysp_new_material(&in.meso, 2, in.phases, &mat);
    check(stat.code == CRYSP_ERR_VAL, "invalid meso model: expected CRYSP_ERR_VAL, got %d", stat.code);
    check(strcmp(stat.proc_name, "meso_get_model") == 0, "invalid meso model: reported by meso_get_model, got '%s'",
          stat.proc_name);
    check(terminated(stat.message, CRYSP_MSG_LEN) && strstr(stat.message, "99") != NULL,
          "invalid meso model: message names the ID, got '%s'", stat.message);
}

static void test_simulation(void) {
    CryspMaterial mat;
    if (!check_ok(new_material(alamel, &mat), "crysp_new_material")) return;

    const CryspHardeningState *hs = &mat.clusters[0].grains[0].hardening_state;
    double crss_before = hs->crss[0];
    double strain_mode[5] = {1., 0., 0., 0., 0.}, stress[5], stress_again[5];

    if (check_ok(crysp_simulate_strain_mode(&mat, strain_mode, stress), "crysp_simulate_strain_mode") &&
        check_ok(crysp_simulate_strain_mode(&mat, strain_mode, stress_again), "crysp_simulate_strain_mode (again)")) {
        check(memcmp(stress, stress_again, sizeof stress) == 0, "simulate_strain_mode: nothing carried back between calls");
        check(hs->crss[0] == crss_before, "simulate_strain_mode: the material is not modified");
    }

    double target[5], residual[5];
    unit(stress, target);
    check_ok(crysp_simulate_stress_mode(&mat, target, strain_mode, stress, residual), "crysp_simulate_stress_mode");
    crysp_destroy_material(&mat);
}

/* Deform mat along a plane strain velocity gradient. Returns the final increment's Taylor factor. */
static int deform(CryspMaterial *mat, const char *label, double *taylor_factor) {
    double v_grad[3][3] = {{1., 0., 0.}, {0., -1., 0.}, {0., 0., 0.}};
    int n_incs = -1;
    CryspStrainIncrement *incs = NULL;

    if (!check_ok(crysp_strain_driven_deformation(mat, v_grad, TARGET_VM_STRAIN, &n_incs, &incs), label)) {
        check(n_incs == 0 && incs == NULL, "%s: no increments on failure", label);
        return 0;
    }
    if (check(n_incs > 0, "%s: returns increments", label)) {
        check(incs[n_incs - 1].n_grains == 2 * N_GRAINS, "%s: texture has one entry per grain", label);
        *taylor_factor = incs[n_incs - 1].taylor_factor;
    }
    for (int i = 0; i < n_incs; i++) crysp_destroy_strain_increment(&incs[i]);
    free(incs);
    return n_incs > 0;
}

static void test_deformation(void) {
    CryspMaterial mat;
    if (!check_ok(new_material(alamel, &mat), "crysp_new_material")) return;

    /* A copy built by the caller with malloc: libCrysP must accept and release it like its own. */
    CryspMaterial copy = copy_material(&mat);
    double crss_before = mat.clusters[0].grains[0].hardening_state.crss[0];

    double taylor_factor, copy_taylor_factor;
    if (deform(&mat, "strain driven deformation", &taylor_factor)) {
        check(mat.clusters[0].grains[0].hardening_state.crss[0] != crss_before,
              "strain driven deformation: the deformed state is written back to the material");
        if (deform(&copy, "strain driven deformation (caller-built material)", &copy_taylor_factor))
            check(near(copy_taylor_factor, taylor_factor, 1e-9),
                  "caller-built material deforms like the original: Taylor factor %.12g vs %.12g", copy_taylor_factor,
                  taylor_factor);
    }
    crysp_destroy_material(&copy);

    /*
     * Target the stress direction the material produces for strain mode (1,0,0,0,0). A hand-picked direction such as
     * (0.6, -0.8, 0, 0, 0) fails in detect_taylor_ambiguity under gfortran, a core issue unrelated to the binding.
     */
    double strain_mode[5] = {1., 0., 0., 0., 0.}, stress[5], target[5];
    if (!check_ok(crysp_simulate_strain_mode(&mat, strain_mode, stress), "crysp_simulate_strain_mode")) {
        crysp_destroy_material(&mat);
        return;
    }
    unit(stress, target);
    int n_incs = -1;
    CryspStressIncrement *incs = NULL;
    if (check_ok(crysp_stress_driven_deformation(&mat, target, 1.5 * TARGET_VM_STRAIN, &n_incs, &incs),
                 "stress driven deformation")) {
        int empty = 0;
        for (int i = 0; i < n_incs; i++) empty += incs[i].n_strain_increments <= 0;
        check(n_incs > 0 && empty == 0, "stress driven deformation: increments made of strain increments");
        for (int i = 0; i < n_incs; i++) crysp_destroy_stress_increment(&incs[i]);
        free(incs);
    }
    crysp_destroy_material(&mat);
}

/* The destroy procedures for parts, on caller-built copies: a stack material with individually heap-allocated parts. */
static void test_destroy_parts(void) {
    CryspMaterial mat;
    MaterialInputs in;
    if (!check_ok(new_material(alamel, &mat), "crysp_new_material")) return;

    CryspCluster cluster = copy_cluster(&mat.clusters[0], mat.phases, mat.phases);
    crysp_destroy_cluster(&cluster);
    CryspGrain grain = mat.clusters[0].grains[0];
    grain.hardening_state = copy_hardening_state(&grain.hardening_state);
    crysp_destroy_grain(&grain);
    CryspHardeningState hardening_state = copy_hardening_state(&mat.clusters[1].grains[0].hardening_state);
    crysp_destroy_hardening_state(&hardening_state);
    CryspModelData model_data = copy_model_data(&mat.phases[0].hardening_model_state);
    crysp_destroy_model_data(&model_data);

    CryspConstitutiveModel model = mat.phases[1];
    model.hardening_model_state = copy_model_data(&model.hardening_model_state);
    model.taylor_coeffs = copy_buffer(model.taylor_coeffs, 5 * (size_t)model.n_systems * sizeof(double));
    model.spin_coeffs = copy_buffer(model.spin_coeffs, 3 * (size_t)model.n_systems * sizeof(double));
    crysp_destroy_constitutive_model(&model);

    make_inputs(&in, alamel, voce);
    CryspPhaseData phase_data = in.phases[0];
    phase_data.model = copy_model_data(&phase_data.model);
    phase_data.orientations = copy_buffer(phase_data.orientations, 3 * N_GRAINS * sizeof(double));
    crysp_destroy_phase_data(&phase_data);

    CryspParameter name = {CRYSP_TYPE_STRING, 0, {4, 1}, copy_buffer("TIVS", 4)};
    crysp_destroy_parameter(&name);

    crysp_destroy_material(&mat);
    check(1, "destroy procedures for parts ran");
}

int main(void) {
    test_layout();
    if (find_models()) {
        test_model_descriptors();
        test_new_material();
        test_errors();
        test_simulation();
        test_deformation();
        test_destroy_parts();
    }

    printf("Ran %d checks, %d failed.\n", n_checks, n_failed);
    return n_failed != 0;
}
