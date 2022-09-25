%% Load Merged data and merge with ratio

load('Merged_data.mat');
formataux = '%f%f%f%f%f%f%f%f'
M = fopen('ratio_80_19.csv');
C = textscan(M, formataux, 'Headerlines', 1, 'Delimiter', ',', 'EmptyValue', NaN, 'TreatAsEmpty', {'A','B','C','P','S','T'}, 'CollectOutput',1);
fclose(M);
ratios = cell2mat(C); 
ratios(:,2)=[];
ratios(:,2)=[];
ratios(:,2)=datenum(num2str(ratios(:,2)),'yyyymmdd');
[~,index_crsp,index_ratios] = intersect(data_crsp(:,[1,2]),ratios(:,[1,2]),'rows');
data_crsp(index_crsp,14)=ratios(index_ratios,3);
data_crsp(index_crsp,15)=ratios(index_ratios,4);
data_crsp(index_crsp,16)=ratios(index_ratios,5);
data_crsp(index_crsp,17)=ratios(index_ratios,6);








c.permno=1;
c.date=2;
c.shrcd=3;
c.exchcd=4;
c.del_ret=5;
c.prc=6;
c.ret=7;
c.shrout=8;
c.cap=9; % In the video some numbers here were wrong. This has no impact on the results.
c.bv=10;
c.bm=11;
c.momentum=12;
c.fut_ret=13;
c.pe=14;
c.fpe=15;
c.fpel=16;
c.arae=17;
% I keep only share codes 10 and 11, and exchange codes 1, 2, 3
filter=intersect(find(ismember(data_crsp(:,c.exchcd),[1,2,3])), find(ismember(data_crsp(:,c.shrcd),[10,11])));
data_crsp=data_crsp(filter,:);

% Sort data based on date - permno
data_crsp = sortrows(data_crsp,[c.date c.permno]);
data_crsp(data_crsp(:,c.bm)==0,c.bm)=NaN;
data_crsp(data_crsp(:,c.pe)==0,c.pe)=NaN;
data_crsp(data_crsp(:,c.fpe)==0,c.fpe)=NaN;
data_crsp(data_crsp(:,c.fpel)==0,c.fpel)=NaN;
data_crsp(data_crsp(:,c.arae)==0,c.arae)=NaN;

save('Merged_Population.mat','data_crsp')

load('Merged_Population.mat');
FY=2002;
FM=1;
LY=2014;
LM=12;

data_crsp=data_crsp(data_crsp(:,c.date)>=eomdate(FY,FM)&data_crsp(:,c.date)<=eomdate(LY,LM),:);

% I keep the vector of months for which I will conduct the analysis
ud=unique(data_crsp(:,c.date));


%% Load FF factors data for JAN 2002 up to DEC 2014 (downloaded from Kenneth French's website)
FF = csvread('F-F_Research_Data_Factors.csv',909,1,'B910..E1065')/100;
ff_factors = FF(:,1:3);
rf = FF(:,4);
%% statistic analysis of PE
data_crsp=data_crsp_1;
data_crsp_1=data_crsp;
pe_stat = data_crsp(:,c.pe);
pe_outlier_1= find(pe_stat>-0.0001);
pe_stat(pe_outlier_1)=nan;
pe_outlier_2= find(pe_stat>50);
pe_stat(pe_outlier_2)=nan;
data_crsp(:,c.pe)=pe_stat
%% statistic stat 
pe_points = prctile(pe_stat, [1 5 20 40 60 80 95 99 100]);
pe_min = min(pe_stat);
pe_max = max(pe_stat);
pe_mean = nanmean(pe_stat);
pe_std = nanstd(pe_stat);

plot(pe_stat)


%%  Simple regression of PE as single risk factor
Sreg=NaN(length(ud),2); % 2 for the constant term and pe
SregR=NaN(length(ud),1);
SregADR=NaN(length(ud),1);
SregMSE=NaN(length(ud),1);
SregT=NaN(length(ud),2);
SregF=NaN(length(ud),2);
SregFP=NaN(length(ud),2);

for i=1:length(ud) % We run a regression for each time period
    tdata=data_crsp(data_crsp(:,c.date)==ud(i),:);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    reg_results = regstats(tdata(:,c.fut_ret), [tdata(:,c.pe)], 'linear', {'beta' 'rsquare' 'adjrsquare' 'mse' 'tstat' 'fstat'});
    Sreg(i,:)=reg_results.beta';
    SregR(i,:)=reg_results.rsquare';
    SregADR(i,:)=reg_results.adjrsquare';
    SregMSE(i,:)=reg_results.mse';
     SregT(i,:)=reg_results.tstat.t';
      SregF(i,:)=reg_results.fstat.f';
      SregFP(i,:)=reg_results.fstat.pval';
end

mean(Sreg);% We get the time-series averages of beta coefficients
mean(Sreg(:,2));
std(Sreg(:,1))/sqrt(156)
mean(SregR);
mean(SregADR);
mean(SregF);
mean(SregFP);
% run a simple t-test for each time-series of betas to see if it is significantly different from zero
[~,p,~,stats] = ttest(Sreg); 

p % P-value
stats.tstat

pe_tstat = tdata(:,c.pe);
pe_tpoints= prctile(pe_tstat, [20 40 60 80 100]);
%% Fama-MacBeth Regressions

fm_betas=NaN(length(ud),4); % 4 for the constant term, size, bm, momentum
fm_R=NaN(length(ud),1);
fm_ADR=NaN(length(ud),1);
fm_F=NaN(length(ud),1);
fm_FP=NaN(length(ud),1);
for i=1:length(ud) % We run a regression for each time period
    tdata=data_crsp(data_crsp(:,c.date)==ud(i),:);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    reg_results = regstats(tdata(:,c.fut_ret), [log(tdata(:,c.cap)), tdata(:,c.pe), tdata(:,c.momentum)], 'linear', {'beta' 'tstat' 'fstat' 'rsquare' 'adjrsquare'});
    fm_betas(i,:)=reg_results.beta';
    fm_R(i,:)=reg_results.rsquare';
    fm_ADR(i,:)=reg_results.adjrsquare';
    fm_F(i,:)=reg_results.fstat.f';
    fm_FP(i,:)=reg_results.fstat.pval';

end

mean(fm_betas); % We get the time-series averages of beta coefficients
std(fm_betas)/sqrt(156);% get the standard error
mean(fm_R); %r square
mean(fm_ADR); %adj r square
[~,p,~,stats] = ttest(fm_betas); % t test

p % P-value
stats.tstat % t-stat

a=mean(fm_R);
b = a*689678/((1-a)*3)
%% Portfolio Performance
% Suppose we want to sort our stocks into 5 quintile portfolios every month
% according to Momentum factor

var=c.pe;

for i=1:length(ud) % We sort portfolios every month
    tdata=data_crsp(data_crsp(:,c.date)==ud(i),:);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % I keep only stocks with available predictor, market cap, and future
    % monthly return data
    q = find(~isnan(tdata(:,c.cap)) & ~isnan(tdata(:,var)) & ~isnan(tdata(:,c.fut_ret)) );
    icap=tdata(q,c.cap);
    ivar=tdata(q,var);
    ireturns=tdata(q,c.fut_ret);
    q_points = prctile(ivar, [20 40 60 80 100]);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % 1st Quintile
    q1 = find(ivar <= q_points(1));
    EW_ret(i,1) = mean(ireturns(q1));
    vw = icap(q1)./repmat(sum(icap(q1)),length(q1),1);
    VW_ret(i,1) = sum(ireturns(q1).*vw);
    % 2nd Quintile
    q2 = find(ivar > q_points(1) & ivar <= q_points(2));
    EW_ret(i,2) = mean(ireturns(q2));
    vw = icap(q2)./repmat(sum(icap(q2)),length(q2),1);
    VW_ret(i,2) = sum(ireturns(q2).*vw);
    % 3rd Quintile
    q3 = find(ivar > q_points(2) & ivar <= q_points(3));
    EW_ret(i,3) = mean(ireturns(q3));
    vw = icap(q3)./repmat(sum(icap(q3)),length(q3),1);
    VW_ret(i,3) = sum(ireturns(q3).*vw);
    % 4th Quintile
    q4 = find(ivar > q_points(3) & ivar <= q_points(4));
    EW_ret(i,4) = mean(ireturns(q4));
    vw = icap(q4)./repmat(sum(icap(q4)),length(q4),1);
    VW_ret(i,4) = sum(ireturns(q4).*vw);
    % 5th Quintile
    q5 = find(ivar > q_points(4) & ivar <= q_points(5));
    EW_ret(i,5) = mean(ireturns(q5));
    vw = icap(q5)./repmat(sum(icap(q5)),length(q5),1);
    VW_ret(i,5) = sum(ireturns(q5).*vw);
end

% Long/Short Portfolio (Buy Q1 / Sell Q5)
LS_EW_ret = EW_ret(:,5) - EW_ret(:,1);
LS_VW_ret = VW_ret(:,5) - VW_ret(:,1);
LS_VW_ret_1 = VW_ret(:,1) -rf

plot(1:length(LS_EW_ret), cumsum(LS_EW_ret), 1:length(LS_VW_ret), cumsum(LS_VW_ret))
plot(1:length(LS_EW_ret), cumprod(1+LS_EW_ret), 1:length(LS_VW_ret), cumprod(1+LS_VW_ret))


%% Descriptive Results for VW portfolio
VW_results(1:5,1) = mean(VW_ret - repmat(rf,1,5))'; %% Average Portfolio Returns above Rf
VW_results(6,1) = mean(LS_VW_ret); %% Average Long/Short Portfolio returns

VW_results(1:5,2) = std(VW_ret - repmat(rf,1,5))'; %% Standard Deviation
VW_results(6,2) = std(LS_VW_ret); %% Standard Deviation of Long/Short Portfolio

VW_results(:,3) = (VW_results(:,1)./VW_results(:,2)).*sqrt(12); %% Sharpe Ratio Annualized

[~,~,~,ret_tstat] =  ttest(VW_ret - repmat(rf,1,5)); 
[~,~,~,ret_tstat_ls] =  ttest(LS_VW_ret);
VW_results(1:5,4) = ret_tstat.tstat'; %% t-stat for portfolio returns significance t-test
VW_results(6,4) = ret_tstat_ls.tstat; %% t-stat for Long/Short portfolio returns significance t-test

% Write the results in an excel file
% xlswrite('VW_Results_MOM.xlsx', VW_results, 'H2:K7')


%% Alpha: Long-short portfolio statistics after adjusting for systematic risk factors
% Run a regression of the long-short portfolios return time-series on the
% systematic risk factors defined by the matrix "ff_factors"

LS_alpha_stats = regstats(LS_VW_ret_1, ff_factors,'linear',{'beta' 'tstat' 'adjrsquare'});