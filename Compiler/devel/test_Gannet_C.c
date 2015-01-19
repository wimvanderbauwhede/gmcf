#include <stdio.h>

/*
This convoluted way of calculating a factorial is 
the proof-of-concept for emitting C from Gannet.

(let
    (assign 'f (lambda 'n 'acc 'f_
                    '(if (< n '2)
                            '(return acc)
                            '(apply (read f_) (- n '1) (* n acc) f_)
                      )
                )
    )
    (apply (read 'f) '5 '1 'f)
)

The compiler translates this into:

r0=>(let r1 r2)
r1=>(assign 'f r3)
r2=>(apply r4 '5 '1 'f)
r3=>(lambda 'n 'acc 'f_ r5)
r4=>(read 'f)
r5=>(if r6 r7 r8)
r6=>(< n '2)
r7=>(return acc)
r8=>(apply r9 r10 r11 f_)
r9=>(read f_)
r10=>(- n '1)
r11=>(* n acc)

assuming all return values are int, in C we have:
*/
static int vmem[4];
static unsigned int f_addr=1;

int r0();
void r1();
int r2();
int r3(); 
int r4(); 
int r5(int,int,unsigned int);
int r6(int);
int r7(int);
int r8(int,int,unsigned int);
int r9(unsigned int);
int r10(int);
int r11(int,int);
int f(int,int,unsigned int);

int main() {    
    int res=r0();
    printf("res:%d\n",res);
    return res;
}    
//r0=>(let r1 r2)
int r0() {
    r1(); 
    return r2();
    }
//r1=>(assign 'f r3)
void r1() {
    vmem[f_addr]=r3();
    }
//r2=>(apply r4 '5 '1 'f)
int r2() {    
        int fpi=r4();
        void* vfp =(void*)fpi;
        int (*fp)(int,int,unsigned int)=vfp;
        return (*fp)(5,1,f_addr);
}    
// r3=>(lambda 'n 'acc 'f_ r5)
int r3() {
    int (*fp)(int,int,unsigned int)=&f;
    void* vfp=(void*)(fp);
    int fpi=(int)vfp;
    return fpi;
}
// r4=>(read 'f)
int r4() {
    return vmem[f_addr];
}

//r5=>(if r6 r7 r8)
int r5(int n, int acc, unsigned int f_) {
    if(r6(n)) {
        return r7(acc);
        } else {
        return r8(n,acc,f_);
        }
    }      
//r6=>(< n '2)
int r6(int n) {
    return (n<2);
    }
//r7=>(return acc)
int r7(int acc) {
    return acc;
    }
//r8=>(apply r9 r10 r11 f_)
int r8(int n, int acc, unsigned int f_) {
        int fpi=r9(f_);
        void* vfp =(void*)fpi;
        int (*fp)(int,int,unsigned int)=vfp;
        return (*fp)(r10(n),r11(n,acc),f_);
}
//r9=>(read f_)
int r9(unsigned int f_) {
    return vmem[f_];
    }
//r10=>(- n '1)
int r10 (int n) {
    return n-1;
    }
//r11=>(* n acc)
int r11 (int n, int acc) {
    return n * acc;
}    

// and finally the lambda function, f
int f(int n, int acc, unsigned int f_) {
    return r5(n,acc,f_);
}

/*
The difficulty is in determining the function signatures. We really need taint checking; the easiest way is to give every function 
inside the lambda the signature of the lambda.
The difference between unsigned int for the addresses and signed int for the return values could be abandoned but I'd prefer to keep it.
I hope I can identify address by the L type
*/
