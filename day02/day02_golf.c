c,n,p,d,a;main(){while(scanf("%c%*s%d\n",&c,&n)>1)c^'d'?a+=n:c^'f'?p+=n,d+=a*n:a-=n;printf("%d %d",p*a,p*d);}
