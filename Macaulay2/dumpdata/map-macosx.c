/* dumpdata for mac os x doesn't work yet, and may never work */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h> 
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <sys/types.h>
#include <unistd.h>

#include "warning.h"
#include "map.h"
#include "file.h"
#include "std.h"

int haveDumpdata() {
  return TRUE;
}

#define count(x) (sizeof(x)/sizeof(int))
#define nest(info) info.is_submap
#define record(info) (!(info.share_mode == SM_COW || info.share_mode == SM_PRIVATE_ALIASED || info.share_mode == SM_EMPTY))

int printmaps() {
  int i=0, nesting_depth=0;
  vm_address_t address = 0;
  struct vm_region_submap_info_64 info;
  vm_size_t size;
  while (address < 0xc0000000) {
    int infoCnt = count(info);
    if (vm_region_recurse_64(mach_task_self(),&address,&size,&nesting_depth,(int *)&info,&infoCnt) != 0) 
      return ERROR;
    if (nest(info)) nesting_depth++;
    else {
      if (record(info)) {
	i++;
	printf("%d: %08x-%08x [%dK] %c%c%c/%c%c%c nesting_depth=%d share_mode=%d is_submap=%c\n",
	       i,address,address+size,size/1024,
	       (info.protection & VM_PROT_READ) ? 'r' : '-',
	       (info.protection & VM_PROT_WRITE) ? 'w' : '-',
	       (info.protection & VM_PROT_EXECUTE) ? 'x' : '-',
	       (info.max_protection & VM_PROT_READ) ? 'r' : '-',
	       (info.max_protection & VM_PROT_WRITE) ? 'w' : '-',
	       (info.max_protection & VM_PROT_EXECUTE) ? 'x' : '-',
	       nesting_depth,
	       info.share_mode,
	       info.is_submap ? 'y' : 'n'
	       );
      }
      address += size;
    }
  }
  {
    char buf[100];
    snprintf(buf,sizeof(buf),"vmmap %d",getpid());
    system(buf);
  }
  return OKAY;
}

int nummaps () {
  int i=0, nesting_depth=0;
  vm_address_t address = 0;
  struct vm_region_submap_info_64 info;
  vm_size_t size;
  printmaps();
  while (address < 0xc0000000) {
    int infoCnt = count(info);
    if (vm_region_recurse_64(mach_task_self(),&address,&size,&nesting_depth,(int *)&info,&infoCnt) != 0) 
      return ERROR;
    if (nest(info)) nesting_depth++;
    else {
      if (record(info)) i++;
      address += size;
    }
  }
  return i;
}

int getmaps(int nmaps, struct MAP maps[nmaps]) {
  int i=0, nesting_depth=0;
  vm_address_t address = 0;
  struct vm_region_submap_info_64 info;
  vm_size_t size;
  while (address < 0xc0000000) {
    int infoCnt = count(info);
    if (vm_region_recurse_64(mach_task_self(),&address,&size,&nesting_depth,(int *)&info,&infoCnt) != 0) 
      return ERROR;
    if (nest(info)) nesting_depth++;
    else {
      if (record(info)) {
	assert(i < nmaps);
	maps[i].from = (void *)address;
	maps[i].to = (void *)(address + size);
	maps[i].r = (info.protection & VM_PROT_READ) != 0;
	maps[i].w = (info.protection & VM_PROT_WRITE) != 0;
	maps[i].x = (info.protection & VM_PROT_EXECUTE) != 0;
	maps[i].checksum = 0;
	maps[i].filename = NULL;
	i++;
      }
      address += size;
    }
  }
  return OKAY;
}

int isNotCheckable() {
     return FALSE;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
