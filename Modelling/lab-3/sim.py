import heapq, math, json
from itertools import count
from collections import deque
from pathlib import Path
import numpy as np

WORK=Path(__file__).resolve().parent
TRACE_PATH=WORK/'modeling_variant.txt'
TRACE=np.array([float(x.replace(',','.')) for x in TRACE_PATH.read_text().split()])
A=TRACE.mean()
Q=(1-math.sqrt(.6))/2
VIS=np.array([4.,2.,1.,3.])
B=(2.,4.5,5.,15.)
C=(1,1,2,2)
R=(.5,.75,.95,.99)

def law(rng, name, mean, n=None):
    if name=='E': return rng.exponential(mean,n)
    if name=='K': return rng.gamma(2,mean/2,n)
    if name=='H':
        branch=rng.random(n)<Q
        means=np.where(branch,mean/(2*Q),mean/(2*(1-Q))) if n else mean/(2*(Q if branch else 1-Q))
        return rng.exponential(means)
    if name=='U': return rng.uniform(0,2*mean,n)
    if name=='D': return mean if n is None else np.full(n,mean)
    if name=='A': return sum(rng.exponential(mean*p/A,n) for p in (107.337420,31.117895,31.117895))
    if name=='T': return np.resize(TRACE,n)*mean/A
    raise ValueError(name)

def sm(seed, n=150000, burn=5000, mean=A, b=2, flow='E', svc='E', cap=(0,3), c=(1,1), split=.6, points=()):
    ra,rb,rc=[np.random.default_rng(x) for x in np.random.SeedSequence(seed).spawn(3)]
    time=np.cumsum(law(ra,flow,mean,n+burn+1))
    service=law(rb,svc,b,n+burn)
    routes=(rc.random(n+burn)>=split).astype(int) if len(c)==2 else np.zeros(n+burn,dtype=int)
    ends=[[0.]*v for v in c]
    lines=[deque() for _ in c]
    waits=[]; times=[]; loss=0
    busy=np.zeros(len(c)); rows=[]
    t0=time[burn]; t1=time[n+burn]
    for j in range(n+burn):
        k=routes[j]; t=time[j]; line=lines[k]; end=ends[k]
        while line and line[0]<=t: line.popleft()
        waiting=end[0]>t
        if waiting and cap[k] is not None and len(line)>=cap[k]:
            if j>=burn: loss+=1
        else:
            start=max(t,end[0]); w=start-t; depart=start+service[j]
            heapq.heapreplace(end,depart)
            if waiting: line.append(start)
            busy[k]+=max(0,min(t1,depart)-max(t0,start))
            if j>=burn:
                waits.append(w); times.append(depart-t)
        step=j+1-burn
        if step in points:
            rows.append((step,loss/step,float(np.mean(waits)) if waits else 0.,float(np.mean(times)) if times else 0.))
    w=np.array(waits); t=np.array(times)
    return dict(loss=loss/n,lost=loss,w=w.mean(),t=t.mean(),sdw=w.std(),sdt=t.std(),
                pos=(w>0).mean(),rho=busy/(t1-t0)/c, x=len(w)/(t1-t0), rows=rows)

def net(seed, m=None, lam=None, b=B, c=C, svc='E', flow='E', dur=600000., burn=60000., sample=False):
    arr,ser,route=[np.random.default_rng(s) for s in np.random.SeedSequence(seed).spawn(3)]
    events=[]; seq=count()
    q=[deque() for _ in c]; busy=[0]*4; stamp=[0.]*4
    qb=np.zeros(4); sb=np.zeros(4); visits=np.zeros(4,dtype=int)
    node_w=[[] for _ in c]; node_t=[[] for _ in c]
    net_w=[]; net_t=[]; exits=0; enters=0; pending=0
    finish=burn+dur
    def area(i,t):
        delta=max(0,t-max(burn,stamp[i]))
        qb[i]+=delta*len(q[i]); sb[i]+=delta*busy[i]; stamp[i]=t
    def begin(i,t,job,at):
        busy[i]+=1
        heapq.heappush(events,(t+law(ser,svc if i==0 else 'E',b[i]),next(seq),i,job,at,t))
    def enter(i,t,job):
        area(i,t)
        if busy[i]<c[i]: begin(i,t,job,t)
        else: q[i].append((job,t))
    if m is not None:
        for _ in range(m): enter(0,0.,[0.,0.])
    else:
        heapq.heappush(events,(law(arr,flow,1/lam),next(seq),-1,None,0.,0.))
    while events and events[0][0]<finish:
        t,_,i,job,at,start=heapq.heappop(events)
        if i==-1:
            enter(0,t,[t,0.]); pending+=1
            if t>=burn: enters+=1
            heapq.heappush(events,(t+law(arr,flow,1/lam),next(seq),-1,None,0.,0.))
            continue
        area(i,t); busy[i]-=1
        if q[i]:
            nxt,nat=q[i].popleft(); begin(i,t,nxt,nat)
        if t>=burn: visits[i]+=1
        w=start-at
        job[1]+=w
        if at>=burn:
            node_w[i].append(w); node_t[i].append(t-at)
        if i==0:
            u=route.random()
            if u<.25:
                if t>=burn: exits+=1
                if job[0]>=burn:
                    net_w.append(job[1]); net_t.append(t-job[0])
                if m is not None:
                    job[:]=[t,0.]; enter(0,t,job)
                else: pending-=1
            else: enter(1 if u<.75 else 2,t,job)
        else: enter(0 if i==3 else 3,t,job)
    for i in range(4): area(i,finish)
    assert sum(len(x) for x in q)+sum(busy)==(m if m is not None else pending)
    rho=sb/dur/c; lq=qb/dur; ln=(qb+sb)/dur; x=exits/dur
    assert np.all(rho<=1+1e-10)
    if m is not None: assert abs(sum(ln)-m)<1e-8
    def cvs(samples):
        return [float(np.std(s)/np.mean(s)) if len(s) and np.mean(s)>0 else 0. for s in samples]
    d=dict(x=x,exits=exits,enters=enters,rho=rho,lq=lq,ln=ln,xn=visits/dur,
           wn=qb/visits,tn=(qb+sb)/visits,w=sum(lq)/x,t=sum(ln)/x,
           cvw=cvs([net_w])[0],cvt=cvs([net_t])[0],cvwn=cvs(node_w),cvtn=cvs(node_t))
    if sample: d['samples']=[net_w,net_t,node_w[0],node_t[0]]
    return d

def avg(run, seeds):
    ds=[run(s) for s in seeds]
    r={k:np.mean([d[k] for d in ds],axis=0) for k in ds[0] if k!='samples'}
    r['reps_x']=[d['x'] for d in ds]
    r['reps_t']=[d['t'] for d in ds]
    if 'samples' in ds[0]:
        r['samples']=[sum((d['samples'][i] for d in ds),[]) for i in range(4)]
    return r

def gordon(mmax,b=B,c=C):
    norm=np.zeros(mmax+1); norm[0]=1.
    for v,s,k in zip(VIS,b,c):
        weights=[1.]
        for j in range(1,mmax+1):
            weights.append(weights[-1]*v*s/min(j,k))
        norm=np.convolve(norm,weights)[:mmax+1]
    return norm[:-1]/norm[1:]

def jackson(lam):
    xs=lam*VIS; rho=xs*np.array(B)/C
    lq=[]
    for x,b,c,r in zip(xs,B,C,rho):
        a=x*b; z=a**c/math.factorial(c)/(1-r)
        pw=z/(sum(a**n/math.factorial(n) for n in range(c))+z)
        lq.append(pw*r/(1-r))
    return dict(t=(sum(lq)+sum(xs*np.array(B)))/lam,w=sum(lq)/lam,rho=rho)

def export(d):
    (WORK/'results.json').write_text(json.dumps(d,default=lambda o:o.tolist()),encoding='utf8')

if __name__=='__main__':
    d={}
    d['base']=avg(lambda s:sm(s,n=220000,mean=1.),range(4201,4205))
    d['flow']={f:{str(r):avg(lambda s:sm(s,b=r*A/.6,flow=f),range(4301,4305)) for r in R} for f in ('T','A','E','U')}
    d['service']={f:{str(r):avg(lambda s:sm(s,b=r*A/.6,svc=f),range(4301,4305)) for r in R} for f in ('E','K','H')}
    points=(10,50,100,200,300,600,1000,2000,5000,10000)
    d['warm']={}
    for f in ('T','A','E'):
        d['warm'][f]={str(r):np.mean([sm(s,n=10000,burn=0,b=r*A/.6,flow=f,points=points)['rows'] for s in range(4401,4465)],axis=0) for r in (.1,.3,.5,.7,.9)}
    d['cap']={str(r):{str(q):avg(lambda s:sm(s,b=r*A/.6,cap=(0,q)),range(4501,4505)) for q in (0,1,3,6,12,24,None)} for r in (.5,.9)}
    d['pool']={str(r):{str(k):avg(lambda s:sm(s,b=r*A*k,c=(k,),cap=(3,)),range(4551,4555)) for k in (1,2,3)} for r in (.5,.9)}
    d['checks']={f:avg(lambda s:sm(s,b=.75*A,c=(1,),cap=(None,),svc=f,n=220000),range(4601,4605)) for f in ('E','K','H')}
    export(d)
    print('SMO done',flush=True)
    xx=gordon(25); bound=1/max(VIS*np.array(B)/C)
    crit=next(i for i,x in enumerate(xx,1) if x>=.98*bound)
    d['exact']=xx; d['crit']=crit
    print('crit',crit,'x',xx[crit-1],flush=True)
    mvals=sorted(set(range(1,crit+3))|{crit+5})
    d['closed']={f:{} for f in ('E','K','H')}
    for f in d['closed']:
        for m in (mvals if f=='E' else [1,3,6,crit,crit+5]):
            d['closed'][f][str(m)]=avg(lambda s:net(s,m=m,svc=f,sample=(m==6)),range(4701,4704))
    d['bottleneck']={k:avg(lambda s:net(s,m=crit,**kw),range(4701,4704)) for k,kw in {'faster':dict(b=(2,4.5,5,6)),'extra':dict(c=(1,1,2,5))}.items()}
    export(d); print('Closed done',flush=True)
    d['open']={}
    for key,lam in [('m3',xx[2]),('m6',xx[5]),('80',.8*bound),('85',.85*bound),('90',.9*bound),('95',.95*bound),('mcrit',xx[crit-1])]:
        dur=4800000 if key=='mcrit' else 1000000
        d['open'][key]={'lambda':lam,'exact':jackson(lam),'sim':avg(lambda s:net(s,lam=lam,dur=dur,burn=dur/10,sample=(key=='m6')),range(4801,4804))}
    d['variant']={}
    for f in ('E','K','H'):
        lam=d['closed'][f]['6']['x']
        d['variant'][f]={'lambda':lam, **{g:avg(lambda s:net(s,lam=lam,dur=1000000,burn=100000,svc=f,flow=g,sample=(f=='H')),range(4901,4904)) for g in ('E','D')}}
    export(d); print('All done',flush=True)

