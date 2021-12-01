b[5000],p,q,n,i;
main(){
for(;~scanf("%d",b+n++););
for(i=n-2;i--;)p+=b[i+1]>b[i];
for(i=n-4;i--;)q+=b[i+3]>b[i];
printf("%d %d",p,q);
}
