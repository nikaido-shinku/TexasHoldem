# TexasHoldem
An Ocaml based Texas Hold'em poker game

# Git rule
1. **Push everything you want to submit to GitHub.**
2. **Document everything**
3. **Test everything before hitting commit**
4. **Remember to check the README file by putting an x between the bracket**
5. **Check that your complete submission is on Cornell's GitHub.**


# Milestones

## Milestone 1

### Satisfactory 
- [x] signature for cards
- [ ] signature for hands
- [x] signature for deck
- [ ] signature for bids 
- [x] implement cards
- [ ] implement hands
- [ ] implement deck
- [ ] implement bids
### Good 
-	[ ] signature for types of hands
- [ ] signature for a hand ranker
- [ ] implement types for hand
- [ ] implement hand ranker
### Excellent
- [ ] signature for a dealer
- [ ] signature for a player
- [ ] signature for the engine
- [ ] implement dealer
- [ ] implement player
- [ ] implement engine


# compile rule
- run **make build** to build the project
- **Note: do not use this functionality yet, I'm not sure if this is correct, since I have not written any testcases** (run **make test** to test the project.)
- run **make** to use utop
- run **make check** to check your environment
- run **make final check** before submitting
- run **make zip** to zip all the ml and mli files
- run **make docs (-public) (-private)** to generate html files for public/private/both documentations
- make clean and make bisect are not supported either, in fact, I'm not sure what they do

# rule for adding new files
- check **_tags** and see if you want to run it with a specific mode such as debug or coverage
- check **.gitignore** and see if there are any unnecessary file created but with types that current .gitignore don't have
- check **.merlin** and see if you introduced any standard package that the project relies on
- check **Makefile** and add your modules into variable **MODULES** 



# Credit
- Most of the prebuild are based on the source code of former CS3110 assignments at Cornell. If there is any violation of academic integrity, I will modify immediately.