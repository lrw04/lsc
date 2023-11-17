#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint8_t mem_t;
typedef uint64_t reg_t;
typedef uint32_t inst_t;

const int pc_id = 31;

typedef struct vm_t {
    // r31: pc
    reg_t reg[32];
    mem_t *mem;
} vm_t;

uint32_t read32_le(mem_t *addr) {
    return (addr[0]) | ((uint32_t)addr[1] << 8) | ((uint32_t)addr[2] << 16) |
           ((uint32_t)addr[3] << 24);
}

uint64_t read64_le(mem_t *addr) {
    return (addr[0]) | ((uint64_t)addr[1] << 8) | ((uint64_t)addr[2] << 16) |
           ((uint64_t)addr[3] << 24) | ((uint64_t)addr[4] << 32) |
           ((uint64_t)addr[5] << 40) | ((uint64_t)addr[6] << 48) |
           ((uint64_t)addr[7] << 56);
}

void write64_le(mem_t *addr, uint64_t val) {
    addr[0] = val >> 0 & 255;
    addr[1] = val >> 8 & 255;
    addr[2] = val >> 16 & 255;
    addr[3] = val >> 24 & 255;
    addr[4] = val >> 32 & 255;
    addr[5] = val >> 40 & 255;
    addr[6] = val >> 48 & 255;
    addr[7] = val >> 56 & 255;
}

double if64(uint64_t x) {
    double r;
    memcpy(&r, &x, sizeof(double));
    return r;
}

uint64_t fi64(double x) {
    uint64_t r;
    memcpy(&r, &x, sizeof(uint64_t));
    return r;
}

int advance(vm_t *vm, uint64_t *res) {
    // for (int i = 0; i < 32; i++) fprintf(stderr, "r%d: %lld\n", i, vm->reg[i]);
    inst_t inst = read32_le(vm->mem + vm->reg[pc_id]);
    // fprintf(stderr, "instruction: %x\n", inst);
    inst_t i = inst >> 27, d = inst >> 22 & 31, a = inst >> 17 & 31,
           b = inst >> 12 & 31, imm = inst & ((1u << 22) - 1);
    vm->reg[pc_id] += 4;
    int64_t ra, rb;
    switch (i) {
            // nop: 0
        case 0:
            break;
            // sys: 1
            // iiii iddd ddii iiii iiii iiii iiii iiii
        case 1:
            *res = vm->reg[d];
            return imm;
            // sdiv smod: 2~
            // iiii iddd ddaa aaab bbbb xxxx xxxx xxxx
        case 2:
            memcpy(&ra, vm->reg + a, 8);
            memcpy(&rb, vm->reg + b, 8);
            vm->reg[d] = ra / rb;
            break;
        case 3:
            memcpy(&ra, vm->reg + a, 8);
            memcpy(&rb, vm->reg + b, 8);
            vm->reg[d] = ra % rb;
            break;
            // add sub mul udiv umod: 4~
            // iiii iddd ddaa aaab bbbb xxxx xxxx xxxx
        case 4:
            vm->reg[d] = vm->reg[a] + vm->reg[b];
            break;
        case 5:
            vm->reg[d] = vm->reg[a] - vm->reg[b];
            break;
        case 6:
            vm->reg[d] = vm->reg[a] * vm->reg[b];
            break;
        case 7:
            vm->reg[d] = vm->reg[a] / vm->reg[b];
            break;
        case 8:
            vm->reg[d] = vm->reg[a] % vm->reg[b];
            break;
            // fadd fsub fmul fdiv: 9~
            // iiii iddd ddaa aaab bbbb xxxx xxxx xxxx
        case 9:
            vm->reg[d] = fi64(if64(vm->reg[a]) + if64(vm->reg[b]));
            break;
        case 10:
            vm->reg[d] = fi64(if64(vm->reg[a]) - if64(vm->reg[b]));
            break;
        case 11:
            vm->reg[d] = fi64(if64(vm->reg[a]) * if64(vm->reg[b]));
            break;
        case 12:
            vm->reg[d] = fi64(if64(vm->reg[a]) / if64(vm->reg[b]));
            break;
            // fti itf 13~
            // iiii iddd ddaa aaax xxxx xxxx xxxx xxxx
        case 13:
            vm->reg[d] = if64(vm->reg[a]);
            break;
        case 14:
            vm->reg[d] = fi64(vm->reg[a]);
            // and or xor not lsh rsh: 15~
            // iiii iddd ddaa aaab bbbb xxxx xxxx xxxx
        case 15:
            vm->reg[d] = vm->reg[a] & vm->reg[b];
            break;
        case 16:
            vm->reg[d] = vm->reg[a] | vm->reg[b];
            break;
        case 17:
            vm->reg[d] = vm->reg[a] ^ vm->reg[b];
            break;
        case 18:
            vm->reg[d] = ~vm->reg[a];
            break;
        case 19:
            vm->reg[d] = vm->reg[a] << vm->reg[b];
            break;
        case 20:
            vm->reg[d] = vm->reg[a] >> vm->reg[b];
            break;
            // ld st ldi: 21~
            // iiii iddd ddaa aaax xxxx xxxx xxxx xxxx
            // iiii iddd ddii iiii iiii iiii iiii iiii
        case 21:
            vm->reg[d] = read64_le(vm->mem + vm->reg[a]);
            break;
        case 22:
            write64_le(vm->mem + vm->reg[d], vm->reg[a]);
            break;
        case 23:
            vm->reg[d] = imm;
            break;
            // mov (jmp): 24
            // iiii iaaa aabb bbbx xxxx xxxx xxxx xxxx
        case 24:
            vm->reg[d] = vm->reg[a];
            break;
            // jnz: 25
            // iiii iaaa aabb bbbx xxxx xxxx xxxx xxxx
        case 25:
            if (vm->reg[a]) vm->reg[pc_id] = vm->reg[d];
            break;
    }
    return 0;
}

void usage(char *progname) {
    fprintf(stderr, "Usage: %s MEMSIZE -- image args ...\n", progname);
    exit(1);
}

int main(int argc, char **argv) {
    char *progname = argv[0];
    if (argc < 4) usage(progname);
    uint64_t size;
    if (!sscanf(argv[1], "%llu", &size)) usage(progname);
    if (strcmp(argv[2], "--")) usage(progname);
    char *img = argv[3];
    mem_t *mem = malloc(size);

    // TODO: load image
    FILE *fp = fopen(img, "r");
    uint64_t img_size;
    fscanf(fp, "%llu", &img_size);
    for (uint64_t i = 0; i < img_size; i++) {
        int x;
        fscanf(fp, "%d", &x);
        mem[i] = x;
    }
    fclose(fp);

    vm_t vm;
    vm.mem = mem;
    vm.reg[pc_id] = 0;
    while (1) {
        reg_t res;
        int status = advance(&vm, &res);
        if (!status) continue;
        switch (status) {
            // hypercall
            case 1:
                exit(0);
            case 2:
                exit(1);
            case 3:
                fprintf(stderr, "debug: %lld\n", res);
                break;
            default:
                abort();
        }
    }
    return 0;
}
