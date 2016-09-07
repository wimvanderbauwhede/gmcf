#ifndef __SPINLOCK__

#define __SPINLOCK__

#include <pthread.h>

#ifdef __APPLE__
#define EBUSY 16
typedef int pthread_spinlock_t;

int pthread_spin_init(pthread_spinlock_t *lock, int pshared);

int pthread_spin_destroy(pthread_spinlock_t *lock);

int pthread_spin_lock(pthread_spinlock_t *lock);

int pthread_spin_trylock(pthread_spinlock_t *lock);

int pthread_spin_unlock(pthread_spinlock_t *lock);
#endif

#endif

