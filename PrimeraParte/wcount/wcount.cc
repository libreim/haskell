#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <algorithm>
#include <locale>

using namespace std;

int main(){
 unordered_map<wstring,int> counter;
 wstring word;

 ios_base::sync_with_stdio(false);
 wcin.imbue(locale("en_US.UTF-8"));
 wcout.imbue(locale("en_US.UTF-8"));

 while (wcin >> word){
   word.erase(remove_if(word.begin(), word.end(), ::iswpunct), word.end());
   transform(word.begin(), word.end(), word.begin(), ::towlower);

   if(word.length() == 0)
     continue;

   auto it = counter.find(word);
   if(it != counter.end())
     it->second += 1;
   else
     counter.emplace(word, 1);
 }

 const int MAX = 200;
 int n = 0, maxLen = 0;

 vector<pair<wstring, int>> words;
 for(auto itr : counter)
   words.push_back(itr);

 sort(words.begin(), words.end(),
      [=](auto a, auto b){
        return a.second > b.second;
      });

 n = 0;
 for(const auto it : words) {
   if(++n > MAX)
     break;
   if(it.first.length() > maxLen)
     maxLen = it.first.length();
 }

 n = 0;
 for (const auto it : words) {
   if(++n > MAX)
     break;
   wcout << setw(maxLen) << it.first << ": " << it.second << endl;
 }
}
