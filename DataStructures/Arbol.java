import java.util.LinkedList;

class NodoArbol {

    NodoArbol nodoIzq;
    int datos;
    NodoArbol nodoDer;


    public NodoArbol(int datosNodo) {
        datos = datosNodo;
        nodoIzq = nodoDer = null;
    }

    public void insertar(int valorInsertar) {
        if(valorInsertar < datos) {
            if(nodoIzq==null) {
                nodoIzq = new NodoArbol(valorInsertar);
            } else {
                nodoIzq.insertar(valorInsertar);
            }
        } else if(valorInsertar > datos) {
            if(nodoDer==null) {
                nodoDer = new NodoArbol(valorInsertar);
            } else {
                nodoDer.insertar(valorInsertar);
            }
        }
    }
}

public class Arbol {

    NodoArbol raiz;

    public Arbol() {
        raiz = null;
    }

    public void insertarNodo(int valorInsertar) {

        if(raiz==null) {
            raiz = new NodoArbol(valorInsertar);
        } else {
            raiz.insertar(valorInsertar);
        }

    }

    public void preorden() {
        recorrePreorden(raiz);
    }

    public void recorrePreorden(NodoArbol nodo) {
        if (nodo==null) {
            return;
        }
        System.out.print(nodo.datos + " ");
        recorrePreorden(nodo.nodoIzq);
        recorrePreorden(nodo.nodoDer);
    }

    public void Inorden(){
        recorreInorden(raiz);
    }

    public void recorreInorden(NodoArbol nodo) { 
        if (nodo==null) {
            return;
        }
        recorreInorden(nodo.nodoIzq);
        System.out.print(nodo.datos + " ");
        recorreInorden(nodo.nodoDer);
    }

    public void Postorden(){
        recorrePostorden(raiz);
    }

    public void recorrePostorden(NodoArbol nodo) { 
        if (nodo==null) {
            return;
        }
        recorrePostorden(nodo.nodoIzq);
        recorrePostorden(nodo.nodoDer);
        System.out.print(nodo.datos + " ");
    }

    public String toString(){

        LinkedList<NodoArbol[]> niveles = new LinkedList<NodoArbol[]>();

        if(this.raiz != null){
            NodoArbol[] nivelRaiz = new NodoArbol[1];
            nivelRaiz[0] = this.raiz;
            niveles.add(nivelRaiz);
            NodoArbol[] nivelHijos = leerNivel(nivelRaiz);

            NodoArbol[] hijosActuales = nivelHijos;
            while(true){
                Boolean hijosVacio = true;
                for(int i=0;i<hijosActuales.length;i++){
                    if(hijosActuales[i] != null){
                        hijosVacio = false;
                    }
                }
                if(!hijosVacio){
                    niveles.add(hijosActuales);
                    hijosActuales = leerNivel(hijosActuales);
                }else{
                    break;
                }
            }

            String arbol = "";
            int numNiveles = niveles.size();
            int espaciado = numNiveles*4;
            int espaciadoInterno = numNiveles*2;
            if(numNiveles <= 5){
                for(int i=0;i<niveles.size();i++){    //recorrer todos los niveles guardados en la lista


                    for(int j=0;j<espaciado;j++){       //insertar espacio inicial en la linea de cada nivel
                        arbol = arbol + " ";
                    }
                    for(int j=0;j<niveles.get(i).length;j++){   //recorrer los nodos guardados en cada nivel y añadirlos a a linea

                        if(niveles.get(i)[j] != null){
                            if(niveles.get(i)[j].datos >= 0 && niveles.get(i)[j].datos < 10){
                                arbol = arbol + niveles.get(i)[j].datos + " ";
                            }else{
                                arbol = arbol + niveles.get(i)[j].datos;
                            }   //imprimir un nodo que si tiene datos
                            
                            for(int k=0;k<espaciadoInterno;k++){
                                arbol = arbol + " ";
                            }
                            
                        
                        }else{
                            arbol = arbol + "   ";  //caso imprimir un nodo vacio
                        }

                    }
                    arbol = arbol + "\n";
                    espaciado -= 4;
                    espaciadoInterno -= 2;
                }
                return arbol;
            }else{

                for(int i=0;i<niveles.size();i++){
                    for(int j=0;j<niveles.get(i).length;j++){
                        if(niveles.get(i)[j] != null){
                            arbol = arbol + niveles.get(i)[j].datos + " ";
                        }

                    }
                    arbol = arbol + "-> ";
                }
                return arbol;


            }


        }else{
            return "el arbol esta vacio";
        }
    }

    //metodo auxiliar para toString y para los metodos auxiliares de imprimeTipo
    public static NodoArbol[] leerNivel(NodoArbol[] nivelPadres){

        boolean nivelVacio = true;
        for(int i=0;i<nivelPadres.length;i++){
            if(nivelPadres[i] != null){
                nivelVacio = false;
                break;
            }
        }
        NodoArbol[] nivelHijos = new NodoArbol[0];

        if(!nivelVacio){
            nivelHijos = new NodoArbol[nivelPadres.length*2];
            for(int i=0,j=0;i<nivelPadres.length;i++,j+=2){
                try{
                    nivelHijos[j] = nivelPadres[i].nodoIzq;
                    nivelHijos[j+1] = nivelPadres[i].nodoDer;
                }catch(NullPointerException e){}
            }
        }

        return nivelHijos;
    }



    public String busquedaSeguimiento(int buscado){
        NodoArbol actual = this.raiz;
        String caminoRecorrido = "";
        while(true){
            if(actual.datos == buscado){
                if(caminoRecorrido.equals("")){
                    caminoRecorrido = caminoRecorrido + "El nodo buscado es la raiz";
                }
                break;
            }else if(actual.datos > buscado){
                actual = actual.nodoIzq;
                caminoRecorrido = caminoRecorrido + "L";
            }else if(actual.datos < buscado){
                actual = actual.nodoDer;
                caminoRecorrido = caminoRecorrido + "D";
            }
            if(actual == null){
                caminoRecorrido = "No se encontro el nodo buscado";
                break;
            }
    
        }

        return caminoRecorrido;
    }

    //metodo auxiliar para getNumeroDescendientes
    public NodoArbol encontrarNodo(int nodo){
        NodoArbol actual = this.raiz;
        NodoArbol encontrado = null;
        while(true){
            if(actual.datos == nodo){
                encontrado = actual;
                break;
            }else if(actual.datos > nodo){
                actual = actual.nodoIzq;
            }else if(actual.datos < nodo){
                actual = actual.nodoDer;
            }
            if(actual == null){
                break;
            }
        }

        return encontrado;

    }

    public int getNumeroDescendientes(int buscado){
        NodoArbol actual = this.encontrarNodo(buscado);
        int contador = 0;

        if(actual != null){
            if(actual.nodoIzq != null){
                contador = contador + 1 + getNumeroDescendientes(actual.nodoIzq.datos);
            }
            if(actual.nodoDer != null){
                contador = contador + 1 + getNumeroDescendientes(actual.nodoDer.datos);
            }
        }else{
            contador = -1;
        }
        return contador;

    }

    public String imprimeTipo(){
        String caracteristicas = "";
        if(esCompleto(this.raiz) == true){
            caracteristicas = caracteristicas + ("Es un arbol binario completo\n");
        }else{
            caracteristicas = caracteristicas + ("No es un arbol binario completo\n");
        }
        if(esPerfecto(this.raiz) == true){
            caracteristicas = caracteristicas + ("Es un arbol binario perfecto\n");
        }else{
            caracteristicas = caracteristicas + ("No es un arbol binario perfecto\n");
        }
        if(esBalanceado(this.raiz) == true){
            caracteristicas = caracteristicas + ("Es un arbol binario balanceado\n");
        }else{
            caracteristicas = caracteristicas + ("No es un arbol binario balanceado\n");
        }
        if(esDegenerado(this.raiz) == true){
            caracteristicas = caracteristicas + ("Es un arbol binario degenerado\n");
        }else{
            caracteristicas = caracteristicas + ("No es un arbol binario degenerado\n");
        }


        return caracteristicas;
    }

    /////////metodos estaticos auxiliares para imprimeTipo//////////

    public static boolean esCompleto(NodoArbol nodo){
        LinkedList<Boolean> revisionNodos = new LinkedList<Boolean>();
 
        if((nodo.nodoIzq == null && nodo.nodoDer == null) || (nodo.nodoIzq != null && nodo.nodoDer != null)){
            if(nodo.nodoIzq != null && nodo.nodoDer != null){
                revisionNodos.add(esCompleto(nodo.nodoIzq));
                revisionNodos.add(esCompleto(nodo.nodoDer));
            }else{
                return true;
            }
            
        }else{
            return false;
        }
        if(revisionNodos.contains(false)){
            return false;
        }else{
            return true;
        }
    }

    public static boolean esPerfecto(NodoArbol nodo){
        LinkedList<NodoArbol[]> niveles = new LinkedList<NodoArbol[]>(); //lista con un arreglo por cada nivel del arbol

        NodoArbol[] nivelRaiz = new NodoArbol[1];
        nivelRaiz[0] = nodo;
        niveles.add(nivelRaiz);
        NodoArbol[] nivelHijos = leerNivel(nivelRaiz);

        NodoArbol[] hijosActuales = nivelHijos;
        while(true){
            Boolean hijosVacio = true;
            for(int i=0;i<hijosActuales.length;i++){
                if(hijosActuales[i] != null){
                    hijosVacio = false;
                }
            }
            if(!hijosVacio){
                niveles.add(hijosActuales);
                hijosActuales = leerNivel(hijosActuales);
            }else{
                break;
            }
        }

        boolean perfecto = true;
        
        for(int i=0;i<niveles.size();i++){
            for(int j=0;j<niveles.get(i).length;j++){
                if(niveles.get(i)[j] == null){
                    perfecto = false;
                    break;
                }
            }
            if(perfecto == false){
                break;
            }
        }

        return perfecto;

    }

    public static boolean esBalanceado(NodoArbol nodo){
        LinkedList<NodoArbol[]> nivelesSubIzq = new LinkedList<NodoArbol[]>();  //lista con un arreglo por cada nivel del subarbol izq
        NodoArbol[] nivelIzq = new NodoArbol[1];
        nivelIzq[0] = nodo.nodoIzq;
        nivelesSubIzq.add(nivelIzq);
        NodoArbol[] nivelHijosIzq = leerNivel(nivelIzq);

        NodoArbol[] hijosActualesIzq = nivelHijosIzq;
        while(true){
            Boolean hijosVacio = true;
            for(int i=0;i<hijosActualesIzq.length;i++){
                if(hijosActualesIzq[i] != null){
                    hijosVacio = false;
                }
            }
            if(!hijosVacio){
                nivelesSubIzq.add(hijosActualesIzq);
                hijosActualesIzq = leerNivel(hijosActualesIzq);
            }else{
                break;
            }
        }

        LinkedList<NodoArbol[]> nivelesSubDer = new LinkedList<NodoArbol[]>();  //lista con un arreglo por cada nivel del subarbol der
        NodoArbol[] nivelDer = new NodoArbol[1];
        nivelDer[0] = nodo.nodoDer;
        nivelesSubDer.add(nivelDer);
        NodoArbol[] nivelHijosDer = leerNivel(nivelDer);

        NodoArbol[] hijosActualesDer = nivelHijosDer;
        while(true){
            Boolean hijosVacio = true;
            for(int i=0;i<hijosActualesDer.length;i++){
                if(hijosActualesDer[i] != null){
                    hijosVacio = false;
                }
            }
            if(!hijosVacio){
                nivelesSubDer.add(hijosActualesDer);
                hijosActualesDer = leerNivel(hijosActualesDer);
            }else{
                break;
            }
        }


        if((nivelesSubIzq.size()-nivelesSubDer.size() > 1) || (nivelesSubDer.size()-nivelesSubIzq.size() > 1)){ //compara si la diferencia de niveles es mas de 1
            return false;
        }else{
            return true;
        }
    }

    public static boolean esDegenerado(NodoArbol nodo){
        if(nodo.nodoIzq != null && nodo.nodoDer != null){
            return false;
        }else if((nodo.nodoIzq != null && nodo.nodoDer == null) || (nodo.nodoIzq == null && nodo.nodoDer != null)){
            boolean degenerado = false;
            if(nodo.nodoIzq != null){
                if(esDegenerado(nodo.nodoIzq)){
                    degenerado = true;
                }else{
                    degenerado = false;
                }
            }else if(nodo.nodoDer != null){
                if(esDegenerado(nodo.nodoDer)){
                    degenerado = true;
                }else{
                    degenerado = false;
                }
            }
            return degenerado;
        }else{
            return true;
        }
    }

}