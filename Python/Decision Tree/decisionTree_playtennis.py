import pandas as pd
import numpy as np

# input attribute and value
# return entropy,and the number of value of attribute
def entropy(attribute, value):
    groupV=df.groupby(by=[attribute,'Play Tennis'])['Play Tennis'].count()[value]
    # if it's pure leaf, entropy set to 0. Otherwise, compute its entropy
    try:
        HN=groupV['No']
    except:
        HN=0
    try:
        HY=groupV['Yes']
    except:
        HY=0 
    
    if HN==0 or HY==0:
        E=0
    else:
        E=-(HY/(HY+HN))*np.log2(HY/(HY+HN))-(HN/(HY+HN))*np.log2(HN/(HY+HN))
        
    return E, HN+HY

# Return the array of Information Gains of all attributes
def igs(attributes):
    N=len(df)
    # compute the entropy for the over all set S
    HY0=df.groupby('Play Tennis').size()['Yes']
    HN0=df.groupby('Play Tennis').size()['No']
    ES=-(HY0/N)*np.log2(HY0/N)-(HN0/N)*np.log2(HN0/N)
    IGs=np.zeros(len(attributes))
    for i in range(0,len(attributes)):
        IG=ES
        # the unique value of attribute
        vs=df[attributes[i]].unique()
        for j in range(0,len(vs)): 
            [E,n]=entropy(attributes[i],vs[j])
            # compute information gain with weighted entropy for each value of the attribute
            IG += -(n/N)*E
        IGs[i]=IG
     
    return IGs

def buildTree(df):
    attrs=df.columns.values[1:5]
    IGs=igs(attrs)
    N=len(df)
    Attr=['' for Attr in range(len(attrs))]
    # Build up the decision tree, and write to a file, dt_result.txt
    f = open("dt_result.txt","w")
    i=0
    j=0
    # Get attribute which is the maximun information gain
    Attr[j]=attrs[np.argsort(IGs)[-(j+1)]]
    # the unique value of the maximun IG attribute
    vs=df[Attr[j]].unique()
    while len(vs)-i > 0:
        # the root node
        groupV=df.groupby(by=[Attr[0],'Play Tennis'])['Play Tennis'].count()[vs[i]]
        # When HY=0, means not play; HN=0 means play
        try:
            HN=groupV['No']
        except:
            isPlay='Yes'
            HN=0
        try:
            HY=groupV['Yes']
        except:
            isPlay='No'
            HY=0 
        # when HN or HY is 0, means it's a pure leaf
        if HN==0 or HY==0:
            N -= (HN+HY)
            f.write('%s: %s -> %s\n' % (Attr[0],vs[i],isPlay))
        else:
            b='%s: %s - ' % (Attr[0],vs[i])
            # Get attribute which is the next maximun information gain, subnodes
            j += 1
            Attr[j]=attrs[np.argsort(IGs)[-(j+1)]]
            groupV=df.groupby(by=[Attr[0],Attr[j],'Play Tennis'])['Play Tennis'].count()[vs[i]]
            # the unique value of attribute,subnode
            Uvalue=df[Attr[j]].unique()
            for r in range(0,len(Uvalue)):
                try:
                    HN=groupV[Uvalue[r],'No']
                except:
                    isPlay='Yes'
                    HN=0
                try:
                    HY=groupV[Uvalue[r],'Yes']
                except:
                    isPlay='No'
                    HY=0 
                
                # when HY or HN is 0, means it's a pure leaf
                if HN==0 or HY==0:
                    N -= (HY+HN)
                    f.write('%s%s: %s -> %s\n' % (b,Attr[j],Uvalue[r],isPlay))
        i += 1

        # when all of the nodes are pure leaf, finish building the decision tree.
        if N == 0:
            f.close()
            break
    
    return


df=pd.read_csv('play tennis.csv')
buildTree(df)    