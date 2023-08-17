#### pelagic-pop-dynamics

# Lotka-Volterra predator-predator-prey simulation

### [Link to the App](https://recfish.pelagic-movement.cloud.edu.au/shiny/ksm723/)

In this simple pelagic ecosystem, the flow of trophic energy looks like this:

```mermaid

%%{
  init: {
    'theme': 'base',
    'themeVariables': {
      'primaryColor': '#BB2528',
      'primaryTextColor': '#fff',
      'primaryBorderColor': '#7C0000',
      'lineColor': '#FF0000',
      'secondaryColor': '#006100',
      'tertiaryColor': '#fff'
    }}
}%%
graph LR
    A(Tuna) --> B(Seal)
    B --> C(White Shark)

 classDef A fill:#D95D39,stroke:#333,stroke-width:2px
 classDef B fill:#310062,stroke:#333,stroke-width:2px
 classDef C fill:#477890,stroke:#333,stroke-width:2px
     class A A
     class B B
     class C C
```

### Three species:  
* **Tuna are the prey**
  * $R$ = Tuna abundance (initially, 2500 individuals)
  * $K$ = the carrying capacity of tuna the environment can support.
  * $r$ = is the intrinsic rate of increase of tuna.
* **Seals eat tunas.**
  * $N$ = seal population
  * $a$ = the encounter rate of seals with tunas. The simulation assumes random movements of both and when encountered tuna are eaten.
  * $f$ = conversion efficiency of tunas into seals. Fixed at a classic 10% (0.1)
  * $q$ = mortality rate of seals. Fixed at 0.05.
* **White Sharks eat seals.** (They don't bother with tunas in this particular ecosystem).
  * $P$ = white shark population
  * $e$ = the encounter rate of white sharks with seals. Same assumptions as with $a$.
  * $z$ = conversion efficiency of seals into white sharks. Fixed at a classic 10% (0.1)
  * $b$ = mortality rate of white sharks. Fixed at 0.05.
    
### The model is described by the following set of differential equations:

$$
\\begin{align*}
\\frac{dR}{dt} &= rR\\left(1 - \\frac{R}{K}\\right) - aNR\\\\
\\frac{dN}{dt} &= f a N R - q N - e N P\\\\  
\\frac{dP}{dt} &= zeNP - bP
\\end{align*}  
$$

So only Tunas have a fixed population growth rate, for seals and white sharks their population growth rates are entirely determined by converting prey into new individuals, minus predations, minus 'natural' mortalities.

### Notes on demonstrable phenomena
  *  The simulations with initial settings result in extinctions of the predators. What changes allow coexistence? What variables don't matter too much?
  *  Once the populations are stable, what happens to the population levels of the three species when the seal attack rate is decreased?
  *  What happens to population levels when the tuna growth rate is increased?
  
 [ I think that last one is the **Paradox of Enrichment** ]: #

### **Wish list**
  * Two level dynamics (without just extincting white shark)
  * Functional responses
  * Smarter/controllable y axes. For now I think the `clip = off` compromise.
 
  [* magnitude of variables if 'realistic']: #

*Inspiration from [Russell Hung's original](https://github.com/RussH-code/Three-Species-Lotka-Volterra-Model/tree/main)*
