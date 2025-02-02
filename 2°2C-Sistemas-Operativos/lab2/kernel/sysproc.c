#include "types.h"
#include "riscv.h"
#include "defs.h"
#include "param.h"
#include "memlayout.h"
#include "spinlock.h"
#include "proc.h"

#define MAX_SEM_VALUE 30
#define true 1
#define false 0


uint64
sys_exit(void)
{
  int n;
  argint(0, &n);
  exit(n);
  return 0;  // not reached
}

uint64
sys_getpid(void)
{
  return myproc()->pid;
}

uint64
sys_fork(void)
{
  return fork();
}

uint64
sys_wait(void)
{
  uint64 p;
  argaddr(0, &p);
  return wait(p);
}

uint64
sys_sbrk(void)
{
  uint64 addr;
  int n;

  argint(0, &n);
  addr = myproc()->sz;
  if(growproc(n) < 0)
    return -1;
  return addr;
}

uint64
sys_sleep(void)
{
  int n;
  uint ticks0;

  argint(0, &n);
  acquire(&tickslock);
  ticks0 = ticks;
  while(ticks - ticks0 < n){
    if(killed(myproc())){
      release(&tickslock);
      return -1;
    }
    sleep(&ticks, &tickslock);
  }
  release(&tickslock);
  return 0;
}

uint64
sys_kill(void)
{
  int pid;

  argint(0, &pid);
  return kill(pid);
}

// return how many clock tick interrupts have occurred
// since start.
uint64
sys_uptime(void)
{
  uint xticks;

  acquire(&tickslock);
  xticks = ticks;
  release(&tickslock);
  return xticks;
}

// sycall simple de hello-world
uint64
sys_hello(void)
{   
    return 0;
}


typedef struct _semaphore
{   
    int count;
    int start;
    int inuse;
    struct spinlock lock;
} semaphore;

semaphore s[MAX_SEM_VALUE];

void init_sem(void) {
    for (unsigned int i = 0; i < MAX_SEM_VALUE; i++) {
        initlock(&s[i].lock, "sem_lock");
        acquire(&s[i].lock);
        s[i].inuse = false;
        release(&s[i].lock);
    }
}

// Abre y/o inicializa el semáforo “sem” con un valor arbitrario “value”
uint64
sys_sem_open(void) 
{   
    int sem, value;
    argint(0, &sem);
    argint(1, &value);

    acquire(&s[sem].lock);
    if (s[sem].inuse == true) {
        release(&s[sem].lock);
        return false;
    }
    s[sem].count = value;
    s[sem].start = value;
    s[sem].inuse = true;
    release(&s[sem].lock);
    return true;
}

// Libera el semáforo “sem”
uint64
sys_sem_close(void)
{   
    int sem;
    argint(0, &sem);
    if (sem < 0||sem >= MAX_SEM_VALUE) { // cheque error
        return false;
    }
    
    acquire(&s[sem].lock);
    s[sem].count = 0;
    s[sem].start = 0; 
    s[sem].inuse = false;   
    release(&s[sem].lock);
    return true;
}

// Incrementa el semáforo ”sem” desbloqueando los procesos cuando su valor es 0
uint64
sys_sem_up(void)  
{   
    int sem;
    argint(0, &sem);
    if (sem < 0||sem >= MAX_SEM_VALUE) { // chequeo error
        return false; 
    }

    acquire(&s[sem].lock);
    s[sem].count++;
    release(&s[sem].lock);
    
    if (s[sem].count >= 1){
        wakeup(&s[sem]);
    }
    
    return true;
}

// Decrementa el semáforo ”sem” bloqueando los procesos cuando su valor es 0. El valor del semaforo nunca puede ser menor a 0
uint64
sys_sem_down(void)  
{   
    int sem;
    argint(0, &sem);
    if (sem < 0|| sem >= MAX_SEM_VALUE) { // chequeo error
        return false;
    }

    acquire(&s[sem].lock);
    while (s[sem].count == 0){
        sleep(&s[sem], &s[sem].lock);
    }
    
    s[sem].count--;
    release(&s[sem].lock);
    return true;
}

