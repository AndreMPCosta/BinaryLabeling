#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

//#define DEBUG_ETIQ

#define MAX_LABELS 99 // número máximo de etiquetas intermédias(>= número máximo de regiões)
#define OBJECT_PIXEL 1
#define BACKGR_PIXEL 0
#define pi atan(1)*4

unsigned int IMAGE_HEIGHT, IMAGE_WIDTH;// número de linhas da imagem // número de colunas da imagem
typedef unsigned int **Image; // tipo de dados – imagem
typedef unsigned int **Labels; // tipo de dados – etiquetas

//variaveis globais(ver uso especifico na funcao onde sao utilizadas)

char namefile[99];//o nome do ficheiro é guardado nestas 3 variáveis(ver main), cada uma é utilizada em 3 funções diferentes, dado que é um valor modificável foi necessário existirem mais duas cópias adicionais.
char namefile1[99];
char namefile2[99];
int gap;//ver funcionamento onde é usada(função clean)

// faz alocação dinâmica de memória para criar uma matriz bidimensional
int ** mallocMat2D(unsigned int nLin, unsigned int nCol)
{
    int ** mat;
    unsigned int i;
    mat=(int **) malloc(nLin*sizeof(int*));
    for (i=0; i<nLin; i++)
        mat[i]=(int *) malloc(nCol*sizeof(int));
    return mat;
}
//limpa a memoria alocada previamente
void freeMat2D(int ** mat, unsigned int nLin)
{
    unsigned int i;
    for (i=0; i<nLin; i++)
        free(mat[i]);
    free(mat);
}

typedef struct// estrutura criada que serve para guardar as equivalencias(a regiao 1 é equivalente à regiao 3, nesse caso seria x = 1 e y = 3)
{
    int x;
    int y;
} equivs;

typedef struct
{
    float x;
    float y;
} massCenter;

typedef struct// estrutura criada que serve para guardar um ponto com as coordenadas x e y
{
    int x;
    int y;
} borders;

float* calcBorders(int number, Labels labels,int *area)//função que calcula os bordos e retorna o peŕimetro das várias regiões da imagem
{
    int i,j,in,in2,region,b,n, move[16]= {0, -1, -1, -1, -1, 0, -1, 1, 0, 1, 1, 1, 1, 0, 1, -1};//vector que guarda as várias translacções do b(explicação detalhada no anexo)
    float *peri;
    Labels aux;//matriz auxiliar que coloca 0's à volta da matriz labels
    aux=mallocMat2D(IMAGE_HEIGHT+2,IMAGE_WIDTH+2);//alocação dinâmica da matriz auxiliar
    peri = (float *) malloc((number+1)*sizeof(float));//alocação dinâmica do vector peri
    borders c,s,temp;//criação das estruturas c, s e temp
    region = in2 = 1;//ins, variaveis auxiliares, para controlar saída de ciclos ou não
    n=0;//inicialização do n
    for(i=0; i<IMAGE_HEIGHT; i++)//cópia da matriz labels para a aux, + alocação de zeros(ler anexo)
    {
        for(j=0; j<IMAGE_WIDTH; j++)
        {
            aux[i+1][j+1]=labels[i][j];
            if(i==0)
                aux[i][j]=0;
            if(i==IMAGE_HEIGHT)
                aux[i+2][j]=0;
        }
    }
//    for(i=0; i<IMAGE_HEIGHT+2; i++) //se for "descomentado" permite a visualização da matriz auxiliar
//    {
//        for(j=0; j<IMAGE_WIDTH+2; j++)
//        {printf("%d ",aux[i][j]);
//        }
//        printf("\n");
//    }

    for(i=1; i<number+1; i++)//inicialização do vector com peri a zeros;
    {
        peri[i]=0;
    }
    while(region < number+1)//ciclo que permite percorrer as várias regiões
    {
        in=1;
        for(i=1; i<IMAGE_HEIGHT+1; i++)//ciclo que permite encontrar o primeiro ponto de uma determinada região
        {
            for(j=1; j<IMAGE_WIDTH+1; j++)
            {
                if(aux[i][j]==region)
                {
                    s.x=i;//s é o primeiro ponto encontrado de determinada região
                    s.y=j;
                    c=s;//c é o ponto actual, que está a ser analisado
                    in=0;
                    break;//depois de encontrar o ponto sai fora do ciclo
                }
                if (in ==0)
                    break;
            }
            if(in==0)
                break;
        }
        do
        {
            if(area[region]==1 || area[region]==0)//se for uma região unitária, ou seja, a região só possui um ponto, ele passa para a região seguinte
            {
                break;
            }
            b=n%8;//permite percorrer de 0 até 7, chegando a 7, recomeça em 0
            if(aux[c.x + move[b*2]][c.y + move[b*2+1]]==region)//aplica a translacção de move e verifica se nesse ponto há um objecto
            {
                temp.x = c.x;//estrutura temporária para guardar o valor de c
                temp.y = c.y;
                c.x=c.x + move[b*2];//actualiza o valor de c
                c.y=c.y + move[b*2+1];
                if (temp.x==c.x || temp.y == c.y)// faz a comparação do c anterior com o actual para saber se o peŕimetro entre os dois pontos é 1, ou raiz de 2
                {
                    peri[region]=peri[region]+1;
                    n=n-2;
                }
                else if (temp.x!=c.x && temp.y!=c.y)
                {
                    peri[region]=peri[region]+sqrt(2);
                    n=n-3;
                }
                in2=0;
            }
            else
            {
                n++;
            }
        }
        while (((c.x != s.x) || (c.y!=s.y)) || in2==1);//condição de paragem, só quando c for igual a s é que passa para a próxima região
        region++;
        in2=1;//reset das variáveis auxiliares
        n=0;
    }
    freeMat2D(aux, IMAGE_HEIGHT+2);//limpa a memória alocada previamente
    return peri;
}

//função que escreve o numero de objectos existentes em determinada imagem e as suas características
int writeobj(char *fil, int number,int connect, Labels labels)//argumentos - 1º - nome do ficheiro da imagem inicial lida(menos ".txt");2º- o numero de objectos existentes na imagem;3º- o tipo de conectivadade a usar, 4º- a matriz labels final(já depois de tudo etiquetado)
{
    int *area,i,j,k;//inicialização das variáveis
    float *perimetro, *fforma;
    massCenter *centroMassa;
    //inicialização das variáveis
    area = (int *) malloc((number+1)*sizeof(int));//alocação dinâmica de memória da variável área, factor de forma e centro de massa
    fforma = (float *) malloc((number+1)*sizeof(float));
    centroMassa = (massCenter *) malloc((number+1)*sizeof(massCenter));
    FILE *file;
    char str[80];//string final que será o nome do ficheiro onde serão guardadas as áreas.
    if (connect == 8)
    {
        strcpy (str,"_c8_obj.txt");//str agora contém _c8_obj.txt a partir do comando strcpy.
    }
    else if (connect==4)
    {
        strcpy (str,"_c4_obj.txt");//str agora contém _c4_obj.txt a partir do comando strcpy.
    }
    strcat (fil,str);//junção de str com o nome do ficheiro de forma a gerar, por exemplo: image01_c4_obj.txt
    file = fopen(fil, "w");//abrir o novo ficheiro no modo write
    //estes dois ciclos for garantem o varrimento da nossa matriz labels em q i fixa as linhas e j fixa as colunas.
    for(i=1; i<number+1; i++)//inicialização da área e do centro de massa a 0
    {
        centroMassa[i].x=0;
        centroMassa[i].y=0;
        area[i]=0;
    }

    for(i=0; i<IMAGE_HEIGHT; i++)//ciclo principal que atribui os centros de massa e área das várias regiões num só varrimento
    {
        for(j=0; j<IMAGE_WIDTH; j++)
        {
            if (labels[i][j]!=0)
            {
                centroMassa[labels[i][j]].x=centroMassa[labels[i][j]].x+i;
                centroMassa[labels[i][j]].y=centroMassa[labels[i][j]].y+j;
                area[labels[i][j]]=area[labels[i][j]]+1;
            }
        }
    }
    for(k=1; k<number+1; k++) //acerto final dos centros de massa depois da área ser calculada
    {
        centroMassa[k].x=centroMassa[k].x/(float)area[k];
        centroMassa[k].y=centroMassa[k].y/(float)area[k];
    }
    perimetro=calcBorders(number,labels,area);//chama a função calcborders para calcular os vários perimetros
    /* fopen returns 0, the NULL pointer, on failure */
    if ( file == 0 )
    {
        printf( "Could not open file\n" );//se não conseguir abrir o ficheiro de escrita por alguma razão(o utilizador pode não ter permissões de escrita no directório que está a tentar escrever,por exemplo), dá uma mensagem de erro
    }
    fprintf(file,"Conetividade: %d\n",connect);//inicio de escrita no ficheiro
    fprintf(file,"Objecto\t\tÁrea\t\  Cooordenadas do centróide\t  Perímetro\t   Factor de forma\n\t\t\t\t\cLin\tcCol\n");
    for (i=1; i<number+1; i++)//ciclo de escrita para percorrer as várias variáveis e escrever no ficheiro
    {
        if(area[i]==0)
        {
        }
        else
        {
            if(perimetro[i]==0)
                fforma[i]=0;
            else
            {
                fforma[i]=(4*pi*area[i])/(perimetro[i]);
            }
            fprintf(file,"|%d|\t\t|%d|\t\t",i,area[i]);
            fprintf(file,"|%.1f|\t",centroMassa[i].x);
            fprintf(file,"|%.1f|\t\t   ",centroMassa[i].y);
            fprintf(file,"|%.1f|\t\t",perimetro[i]);
            fprintf(file,"|%.1f|\n",fforma[i]);
        }
    }
    fclose(file);//fecha o ficheiro
    return 0;//se não houver nenhum erro a executar esta função, ela retorna 0

}


int findmax(int a[])// função genérica, que recebe um vector e retorna o seu máximo(não é assim tão genérica porque o vector recebido terá sempre o tamanho de MAX_LABELS)
{
    int i;
    int max = a[0];
    int min = a[0];

    for (i = 0; i < MAX_LABELS; i++)
    {
        if (a[i] > max)
        {
            max = a[i];
        }
        else if (a[i] < min)
        {
            min = a[i];
        }
    }

    return max;
}

//função clean - digamos que limpa a matriz labels depois de já terem sido etiquetadas as várias regiões. Por vezes certas imagens levam a que desapareçam certas regiões e esta função corrige essa falta( se faltar uma região 2, ela passa todas as regiões 3 a 2)
int clean(Labels labels)
{
    int i,j,h,label,found,foundlast,newfound,checkbreak,doublebreak,index[MAX_LABELS];
    gap=0;//inicialização de gap a 0, variável que conta quantos gaps existem numa imagem(se por exemplo existir uma região 2 e 5 e nada entre elas, um gap será contabilizado, pois quando tentar passar a região 4 a 3 não irá conseguir, pois faltam ambas)
    foundlast = 0;//variável que guarda a ultima label existente na matriz labels
    h=0;
    for(i=0; i<MAX_LABELS; i++)
    {
        index[i]=0;//preenchimento do vector index com 0's com MAX_LABELS elementos
    }
    for(i=IMAGE_HEIGHT-1; i>0; i--)//varrimento da matriz labels de baixo para cima
    {
        for(j=IMAGE_WIDTH-1; j>0; j--)
        {
            if (labels[i][j]!=0)
            {
                index[h]=labels[i][j];//guarda todos os elementos que sejam diferentes de 0 no vector index
                h++;
            }
        }
    }
    foundlast=findmax(index);//calcula a label máxima recorrendo à função findmax
    label=1;//inicializa a variável label
    while(label<foundlast)//ciclo de escrita que começa na primeira label e só pára na label máxima que existir na imagem
    {
        checkbreak=doublebreak=0;//variáveis que vão servir para "saltar fora" de dois ciclos
        found=0; //variável que guarda se um elemento foi descoberto na matriz ou não(sim - 1, não - 0)
        newfound=0; //variável que guarda se o indice do elemento seguinte pode ser decrementado em uma unidade ou não(sim - 1, não - 0)

        for(i=0; i<IMAGE_HEIGHT; i++)
        {
            for(j=0; j<IMAGE_WIDTH; j++)
            {
                if (labels[i][j]==label)//se encontrar a label q procuramos faz found = 1, incrementa a variável label, e faz o primeiro "salta fora"
                {
                    found=1;
                    label++;
                    checkbreak=1;
                    break;
                }
                if (checkbreak==1)//se tem que saltar pois check break é igual a 1, faz o segundo "salta fora"
                {
                    doublebreak=1;
                    break;
                }
                if(i==IMAGE_HEIGHT-1 && j == IMAGE_WIDTH-1 && found ==0) //se chegar ao fim da matriz e ainda não tiver encontrado a label
                {
                    for(i=0; i<IMAGE_HEIGHT; i++)
                    {
                        for(j=0; j<IMAGE_WIDTH; j++)//percorre novamente a matriz
                        {
                            if(labels[i][j]==label+1)//tenta procurar a label seguinte, se encontrar faz a substituição pela label que está em falta
                            {
                                labels[i][j]=label;
                                newfound = 1;//conseguiu encontrar a label seguinte
                            }

                            if(i==IMAGE_HEIGHT-1 && j == IMAGE_WIDTH-1 && newfound ==0)//se depois disto tudo nem found nem newfound forem 1, então quer dizer que estamos perante um gap, e tal é registado. Dependendo do número de gaps encontrados, esta função será chamada novamente o número de vezes igual a gap(ver connect8 ou connect4), para continuar a fazer "clean"(processo recursivo)
                            {
                                gap++;
                                label++;
                                newfound=0;
                            }
                        }
                    }
                }
            }
            if(doublebreak==1)//se saltou fora do 2º break, faz finalmente o terceiro "salta fora"
                break;
        }
    }
    return foundlast;//retorna o numero máximo de labels, o que se vai tornar útil na chamada de outras funções.
}

//função que processa as equivalências
int rescan(Labels labels, equivs save_equivs[])
{
    int i,k,j,h,l,max;
    int *newsave;
    max=save_equivs[99].y; //inializa max no ultimo elemento do vector save_equivs
    //calculo do maximo das equivalencias
    for (i=MAX_LABELS; i>0; i--)
    {
        if(save_equivs[i].x>max)
            max=save_equivs[i].x;
        else if(save_equivs[i].y>max)
            max=save_equivs[i].y;
    }
    l=h=max+1;
    newsave = (int *) malloc((max+1)*sizeof(int));
    //criação do vector que vai guardar as equivalências finais e uma espécie de alocação dinâmica de memória
    for(i=0; i<max+1; i++)
    {
        newsave[i]=0;//inicializar todos os indices a 0
    }
    for(i=MAX_LABELS; i>-1; i--)//percorrer o vector save_equivs onde todas as equivalencias estão guardadas(do fim para o inicio que se percorre)
    {
        if(save_equivs[i].x<save_equivs[i].y && save_equivs[i].x!=0 && save_equivs[i].y!=0)//ler explicação detalhada que se encontra no pdf
        {
            if(newsave[save_equivs[i].x]==0)
                newsave[save_equivs[i].x]=save_equivs[i].y;
            else if(newsave[save_equivs[i].x]>save_equivs[i].y)
            {
                newsave[save_equivs[i].y]=newsave[save_equivs[i].x];
                newsave[save_equivs[i].x]=save_equivs[i].y;
            }
        }
        else if (save_equivs[i].x>save_equivs[i].y && save_equivs[i].x!=0 && save_equivs[i].y!=0)
        {
            if(newsave[save_equivs[i].x]==0)
                newsave[save_equivs[i].y]=save_equivs[i].x;
            else if(newsave[save_equivs[i].x]>save_equivs[i].y)
            {
                newsave[save_equivs[i].y]=save_equivs[i].x;
            }
        }
    }

#ifdef DEBUG_ETIQ//se for definida esta macro no inicio do programa, esta parte do codigo executa, e mostra as equivalências
    printf("A lista de equivalencias e':\n");

    for(i=0; i<max+1; i++)
    {
        if(newsave[i]!=0)
            printf("%d e' equivalente a %d\n",i, newsave[i]);
    }
#endif
    while(h>0)//processamento se existirem equivalências iguais, por exemplo(exemplo 1): 1 equivalente a 5, 2 equivalente a 5, 3 equivalente a 5, 4 equivalente a 5
    {
        for(i=h-1; i>-1; i--)//percorre o vector criado anteriormente(newsave)
        {
            if (i < newsave[i] && newsave[i]!=0)//verifica se o conteudo é menor que o indice e se é diferente de 0
            {
                for(k=l-2; k>0; k--)//parte que serve para passar as equivalências a(exemplo 1): 1 equivalente a 2, 2 equivalente a 3, 3 equivalente a 4 e 4 equivalente a 5
                {
                    if (newsave[k]==newsave[i] &&(k!=i))
                        newsave[k]=i;
                }
            }
            l--;
        }
        h--;
    }

    l=max;
    while(l>0)
    {
        for(k=IMAGE_HEIGHT-1; k>-1; k--)//percorre a matriz labels de baixo para cima, e faz o processamento das equivalências
        {
            for(j=IMAGE_WIDTH-1; j>-1; j--)
            {
                if (labels[k][j]==newsave[l] && newsave[l]!=0)
                {
                    labels[k][j]=l;
                }
            }
        }
        l--;
    }
//     for(i=0; i<IMAGE_HEIGHT; i++)
//        {
//            for(j=0; j<IMAGE_WIDTH; j++)
//            {
//                printf("-%d-",labels[i][j]);
//            }
//        }

    return 0;
}

//função que escreve as etiquetas intermédias da conectividade 8 num ficheiro
int write8f(char *fil, Labels labels)
{
    int i,j;
    FILE *file;
    char str[80];
    strcpy (str,"_c8_ei.txt");
    strcat (fil,str);
    file = fopen(fil, "w");
    /* fopen returns 0, the NULL pointer, on failure */
    if ( file == 0 )
    {
        printf( "Could not open file\n" );
    }
    for (i=0; i<IMAGE_HEIGHT; i++)
    {
        for (j=0; j<IMAGE_WIDTH; j++)
        {
            fprintf(file,"%d ",labels[i][j]);//vai escrevendo os vários elementos da matriz labels
            if (j==IMAGE_WIDTH-1)
                fprintf(file,"\n");//faz enter quando está na última coluna
        }
    }
    fclose(file);
    return 0;
}

//funções análogas a write8f
//função que escreve as etiquetas finais da conectividade 8 num ficheiro
int write8fi(char *fil, Labels labels)
{
    int i,j;
    FILE *file;
    char str[80];
    strcpy (str,"_c8_ef.txt");
    strcat (fil,str);
    file = fopen(fil, "w");
    /* fopen returns 0, the NULL pointer, on failure */
    if ( file == 0 )
    {
        printf( "Could not open file\n" );
    }
    for (i=0; i<IMAGE_HEIGHT; i++)
    {
        for (j=0; j<IMAGE_WIDTH; j++)
        {
            fprintf(file,"%d ",labels[i][j]);
            if (j==IMAGE_WIDTH-1)
                fprintf(file,"\n");
        }
    }
    fclose(file);
    return 0;
}

//função que escreve as etiquetas intermédias da conectividade 4 num ficheiro
int write4f(char *fil, Labels labels)
{
    int i,j;
    FILE *file;
    char str[80];
    strcpy (str,"_c4_ei.txt");
    strcat (fil,str);
    file = fopen(fil, "w");
    /* fopen returns 0, the NULL pointer, on failure */
    if ( file == 0 )
    {
        printf( "Could not open file\n" );
    }
    for (i=0; i<IMAGE_HEIGHT; i++)
    {
        for (j=0; j<IMAGE_WIDTH; j++)
        {
            fprintf(file,"%d ",labels[i][j]);
            if (j==IMAGE_WIDTH-1)
                fprintf(file,"\n");
        }
    }
    fclose(file);
    return 0;
}

//função que escreve as etiquetas finais da conectividade 4 num ficheiro
int write4fi(char *fil, Labels labels)
{
    int i,j;
    FILE *file;
    char str[80];
    strcpy (str,"_c4_ef.txt");
    strcat (fil,str);
    file = fopen(fil, "w");
    /* fopen returns 0, the NULL pointer, on failure */
    if ( file == 0 )
    {
        printf( "Could not open file\n" );
    }
    for (i=0; i<IMAGE_HEIGHT; i++)
    {
        for (j=0; j<IMAGE_WIDTH; j++)
        {
            fprintf(file,"%d ",labels[i][j]);
            if (j==IMAGE_WIDTH-1)
                fprintf(file,"\n");
        }
    }
    fclose(file);
    return 0;
}

//função que analisa uma imagem passada pela utilizador e faz a devida etiquetagem com conectividade 8
int connect8(Image image)
{
    int i,j,label,flag,count,number;
    equivs save_equivs[MAX_LABELS];
    Labels labels;
    labels=mallocMat2D(IMAGE_HEIGHT,IMAGE_WIDTH);//alocação dinâmica de memória para a matriz labels
    equivs e;//cria uma estrutura do tipo equivs
    label = 1; //inicialização do label a 1
    count = 0;
    for(i=0; i<MAX_LABELS; i++)//põe todos os elementos do vector save_equivs a 0(x e y)
    {
        e.x=0;
        e.y=0;
        save_equivs[i]=e;
    }

    for(i=0; i<IMAGE_HEIGHT; i++)//percorre a imagem, i fixa linhas, j fixa colunas
    {
        for(j=0; j<IMAGE_WIDTH; j++)
        {
            flag = 0;//sistema de flag, ela é inicializada a 0, cada vez que começa a analizar um novo elemento
            if(image[i][j]==BACKGR_PIXEL) //verifica se o ponto que está a ser analizado é background, se for, na matriz labels esse ponto também passa a ser background
                labels[i][j]=BACKGR_PIXEL;
            if (i==0 && j ==0 && image[i][j]==OBJECT_PIXEL)//caso especial de bordos, explicado no pdf
            {
                labels[i][j]=label;
                label++;
            }

            if(image[i][j]==OBJECT_PIXEL && (j!=0 || i!=0))//verificação se neste ponto temos um objecto
            {
                if(i!=0 && j!=0 && image[i][j-1]==BACKGR_PIXEL && image[i-1][j-1]==BACKGR_PIXEL && image[i-1][j]==BACKGR_PIXEL && image[i-1][j+1]==BACKGR_PIXEL)//verifica se todos os elementos à volta do elemento a ser analisado possuem background, se sim a variável label é incrementada
                {
                    labels[i][j]=label;
                    label++;
                }
                if(i==0 && j!=0 && image[i][j-1]==BACKGR_PIXEL)//caso bordos(ler pdf)
                {
                    labels[i][j]=label;
                    label++;
                }
                if(i!=0 && j==0 && image[i-1][j]==BACKGR_PIXEL && image[i-1][j+1]==BACKGR_PIXEL)//caso bordos(ler pdf)
                {
                    labels[i][j]=label;
                    label++;
                }
                if(i!=0 && j==IMAGE_WIDTH-1 && image[i][j-1]==BACKGR_PIXEL && image[i-1][j-1]==BACKGR_PIXEL && image[i-1][j]==BACKGR_PIXEL)//caso bordos (ler pdf)
                {
                    labels[i][j]=label;
                    label++;
                }

                if(i!=0 && j!=IMAGE_WIDTH-1 && image[i-1][j+1]==OBJECT_PIXEL)//analisa se o ponto D possui um objecto, e soma 8 à flag, a ordem de se ter começado pelo D não é aleatória dado que no trabalho era pedido que o valor guardado fosse o 1º a ser visto(a começar por A), assim sendo, A é analisado em último pois assim faz overwrite caso D tivesse um objecto
                {
                    labels[i][j]=labels[i-1][j+1];
                    flag = flag + 8;
                }
                if(i!=0 && image[i-1][j]==OBJECT_PIXEL)// analisa se o ponto C possui um objecto, soma 4 à flag, e guarda o valor em labels
                {
                    labels[i][j]=labels[i-1][j];
                    flag = flag + 4;
                }
                if((j!=0 && i!=0) && image[i-1][j-1]==OBJECT_PIXEL)// analisa se o ponto B possui um objecto, soma 2 à flag, e guarda o valor em labels
                {
                    labels[i][j]=labels[i-1][j-1];
                    flag=flag + 2;
                }
                if(j!=0 && image[i][j-1]==OBJECT_PIXEL)//analisa se o ponto A possui um objecto, soma 2 à flag, e guarda o valor em labels
                {
                    labels[i][j]=labels[i][j-1];
                    flag = flag + 1;
                }
                //combinações possiveis da flag, e guarda as equivalências no vector save_equivs
                // A = [i][j-1]
                // B = [i-1][j-1]
                // C = [i-1][j]
                // D = [i-1][j+1]
                if(flag == 3)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j-1];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 5)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 9)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 6)
                {
                    e.x=labels[i-1][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 10)
                {
                    e.x=labels[i-1][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
                if (flag == 12 )
                {
                    e.x=labels[i-1][j+1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 7)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j-1];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 11)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j-1];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 13)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 14)
                {
                    e.x=labels[i-1][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i-1][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
                if(flag == 15)
                {
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j-1];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j];
                    save_equivs[count]=e;
                    count++;
                    e.x=labels[i][j-1];
                    e.y=labels[i-1][j+1];
                    save_equivs[count]=e;
                    count++;
                }
            }
        }
    }
    write8f(namefile,labels);//chama a função para escrever as etiquetas intermédias
    rescan(labels,save_equivs);//chama a função que processa as equivalências
    number=clean(labels);//chama a função que faz a correcção à matriz labels
    while(gap>-1)//enquanto existirem gaps, há nova correcção
    {
        number=clean(labels);
        gap--;
    }
    writeobj(namefile2,number,8,labels);//chama a função de escrita da área dos objectos
    write8fi(namefile1,labels);// chama a função de escrita das etiquetas finais
    freeMat2D(labels, IMAGE_HEIGHT);//limpa a memória alocada previamente
    return 0;//retorna 0 se for bem sucedida
}

//função análoga a connect8, só muda a forma de analisar a vizinhança, apenas considerando A e C
int connect4(Image image)
{
    int i,j,label,flag,count,number;
    equivs save_equivs[MAX_LABELS];
    Labels labels;
    labels=mallocMat2D(IMAGE_HEIGHT,IMAGE_WIDTH);
    equivs e;
    label = 1; //inicialização das etiquetas
    count = 0;
    for(i=0; i<MAX_LABELS; i++)
    {
        e.x=0;
        e.y=0;
        save_equivs[i]=e;
    }

    for(i=0; i<IMAGE_HEIGHT; i++)
    {
        for(j=0; j<IMAGE_WIDTH; j++)
        {
            flag=0;
            if(image[i][j]==BACKGR_PIXEL) //verifica se o ponto que está a ser analisado é background, se for, na matriz labels esse ponto também passa a ser background
                labels[i][j]=BACKGR_PIXEL;
            else
            {
                if (i==0 && j ==0 && image[i][j]==OBJECT_PIXEL)
                {
                    labels[i][j]=label;
                    label++;
                }
                if((j!=0 || i!=0) && image[i][j]==OBJECT_PIXEL)
                {
                    if(i!=0 && j!=0 && image[i][j-1]==BACKGR_PIXEL && image[i-1][j]==BACKGR_PIXEL)
                    {
                        labels[i][j]=label;
                        label++;
                    }
                    if(j!=0 && i==0 && image[i][j-1]==BACKGR_PIXEL)
                    {
                        labels[i][j]=label;
                        label++;
                    }
                    if(j==0 && i!=0 && image[i-1][j]==BACKGR_PIXEL)
                    {
                        labels[i][j]=label;
                        label++;
                    }
                    if(i!=0 && image[i-1][j]==OBJECT_PIXEL )
                    {
                        labels[i][j]=labels[i-1][j];
                        flag = flag + 2;
                    }
                    if(j!=0 && image[i][j-1]==OBJECT_PIXEL)
                    {
                        labels[i][j]=labels[i][j-1];
                        flag = flag + 1;
                    }
                    if(flag == 3)
                    {
                        e.x=labels[i][j-1];
                        e.y=labels[i-1][j];
                        save_equivs[count]=e;
                        count++;
                    }
                }
            }
        }
    }
    write4f(namefile,labels);
    rescan(labels,save_equivs);
    number=clean(labels);
    while(gap>-1)
    {
        number=clean(labels);
        gap--;
    }
    writeobj(namefile2,number,4,labels);
    write4fi(namefile1,labels);
    freeMat2D(labels, IMAGE_HEIGHT);//limpa a memória alocada previamente
    return 0;
}


int main ( int argc, char *argv[] )
{
    int j,i,x;
    j=i=0;
    Image save;
    // tipo de dados – etiquetas
    if ( argc < 2 ) /* argc deve ser 2 para uma execução correcta */
    {
        /* We print argv[0] assuming it is the program name */
        printf( "Insira o nome de um ficheiro que contenha uma imagem binaria");
        exit(1);
    }
    else
    {
        // We assume argv[1] is a filename to open
        FILE *file = fopen( argv[1], "r" );
        strcpy (namefile,argv[1]);//guarda em namefile o nome do ficheiro que contém a imagem
        strcpy (namefile1,argv[1]);
        strcpy (namefile2,argv[1]);
        strncpy (namefile,namefile,(strlen(namefile)-4));//retira o ".txt"
        namefile[strlen(namefile)-4]='\0';
        strncpy (namefile1,namefile1,(strlen(namefile1)-4));
        namefile1[strlen(namefile1)-4]='\0';
        strncpy (namefile2,namefile2,(strlen(namefile2)-4));
        namefile2[strlen(namefile2)-4]='\0';
        /* fopen returns 0, the NULL pointer, on failure */
        if ( file == 0 )
        {
            printf( "Nao e' possivel abrir o ficheiro\n" );
        }

        else
        {
            fscanf(file,"%d %d",&IMAGE_HEIGHT, &IMAGE_WIDTH);
            save=mallocMat2D(IMAGE_HEIGHT,IMAGE_WIDTH);
            /* Lê caracter a caracter do ficheiro, pára no fim do ficheiro(EOF)*/
            fseek ( file , ftell(file)+2 , SEEK_SET );
            while  ( ( x = fgetc( file ) ) != EOF )
            {
                if(x == '\n')
                {
                    //printf("%c",x);
                    i++;
                    j = 0;
                }
                else
                {
                    save[i][j] = x - 48;
                    //printf("%d",save[i][j]);
                    j++;
                }
            }
            fclose( file );
            if (argc == 2)//se o utilizador so introduzir o nome do ficheiro sem conectividade, é executada a conectividade 4
                connect4(save);
            else
            {
                //verifica a conectividade escolhida
                if(atoi(argv[2])==4)
                    connect4(save);
                if(atoi(argv[2])==8)
                    connect8(save);
                if(atoi(argv[2])!=4 && atoi(argv[2])!=8 && argc>=3)//caso especial
                    printf("O programa nao executa esse tipo de conectividade");
            }
        }
    }
    freeMat2D(save, IMAGE_HEIGHT);//limpa a memória alocada previamente
    return 0;
}
