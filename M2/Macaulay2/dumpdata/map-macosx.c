#include <assert.h> 
#include <stdio.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#include "warning.h"
#include "map.h"
#include "file.h"
#include "std.h"

int haveDumpdata() {
  return TRUE;
}

#define count(x) (sizeof(x)/sizeof(int))

int nummaps () {
  int i=0, nesting_depth=0;
  vm_address_t address = 0;
  struct vm_region_submap_info_64 info2;
  vm_size_t size;
  while (address < 0xc0000000) {
    int infoCnt = count(info2);
    if (vm_region_recurse_64(mach_task_self(),&address,&size,&nesting_depth,(int *)&info2,&infoCnt) != 0) 
      return ERROR;
    if (info2.is_submap) {
      nesting_depth++;
    }
    else {
      fprintf(stderr,
	      "%d: %08x-%08x %c%c%c/%c%c%c nesting_depth=%d is_submap=%c\n",
	      i,address,address+size,
	      (info2.protection & VM_PROT_READ) ? 'r' : '-',
	      (info2.protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info2.protection & VM_PROT_EXECUTE) ? 'x' : '-',
	      (info2.max_protection & VM_PROT_READ) ? 'r' : '-',
	      (info2.max_protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info2.max_protection & VM_PROT_EXECUTE) ? 'x' : '-',
	      nesting_depth,
	      info2.is_submap ? 'y' : 'n'
	      );
      i++;
      address += size;
    }
  }
  return i;
}

int getmaps(int nmaps, struct MAP maps[nmaps]) {
  int i=0, nesting_depth=0;
  vm_address_t address = 0;
  struct vm_region_submap_info_64 info2;
  vm_size_t size;
  while (address < 0xc0000000) {
    int infoCnt = count(info2);
    if (vm_region_recurse_64(mach_task_self(),&address,&size,&nesting_depth,(int *)&info2,&infoCnt) != 0) 
      return ERROR;
    if (info2.is_submap) {
      nesting_depth++;
    }
    else {
      fprintf(stderr,
	      "%d: %08x-%08x %c%c%c/%c%c%c nesting_depth=%d is_submap=%c\n",
	      i,address,address+size,
	      (info2.protection & VM_PROT_READ) ? 'r' : '-',
	      (info2.protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info2.protection & VM_PROT_EXECUTE) ? 'x' : '-',
	      (info2.max_protection & VM_PROT_READ) ? 'r' : '-',
	      (info2.max_protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info2.max_protection & VM_PROT_EXECUTE) ? 'x' : '-',
	      nesting_depth,
	      info2.is_submap ? 'y' : 'n'
	      );
      assert(i < nmaps);
      maps[i].r = (info2.protection & VM_PROT_READ) != 0;
      maps[i].w = (info2.protection & VM_PROT_WRITE) != 0;
      maps[i].x = (info2.protection & VM_PROT_EXECUTE) != 0;
      maps[i].checksum = 0;
      i++;
      address += size;
    }
  }
  return OKAY;
}
