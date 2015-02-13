using System;
using MicrosoftResearch.Infer;
using MicrosoftResearch.Infer.Distributions;
using MicrosoftResearch.Infer.Factors;

namespace MicrosoftResearch.Infer.Models.User
{
	/// <summary>
	/// Generated algorithm for performing inference
	/// </summary>
	/// <remarks>
	/// The easiest way to use this class is to wrap an instance in a CompiledAlgorithm object and use
	/// the methods on CompiledAlgorithm to set parameters and execute inference.
	/// 
	/// If you instead wish to use this class directly, you must perform the following steps:
	/// 1) Create an instance of the class
	/// 2) Set the value of any externally-set fields e.g. data, priors
	/// 3) Call the Execute(numberOfIterations) method
	/// 4) Use the XXXMarginal() methods to retrieve posterior marginals for different variables.
	/// 
	/// Generated by Infer.NET 2.5 at 4:43 PM on Friday, February 13, 2015.
	/// </remarks>
	public partial class Model25_EP : IGeneratedAlgorithm
	{
		#region Fields
		/// <summary>Field backing the NumberOfIterationsDone property</summary>
		private int numberOfIterationsDone;
		/// <summary>Field backing the cst property</summary>
		private int Cst;
		/// <summary>Field backing the obs1 property</summary>
		private bool[] Obs1;
		/// <summary>Field backing the cst1 property</summary>
		private double Cst1;
		/// <summary>Field backing the cst0 property</summary>
		private double Cst0;
		/// <summary>The number of iterations last computed by Changed_cst_obs1_cst1_cst0. Set this to zero to force re-execution of Changed_cst_obs1_cst1_cst0</summary>
		public int Changed_cst_obs1_cst1_cst0_iterationsDone;
		public PointMass<int> cst_marginal;
		public DistributionStructArray<Bernoulli,bool> obs1_marginal;
		public Gaussian cst1_marginal;
		public Gaussian cst0_marginal;
		/// <summary>Message to marginal of 'BetaSample0'</summary>
		public Beta BetaSample0_marginal_F;
		public DistributionStructArray<Bernoulli,bool> BernoulliSample0_marginal;
		/// <summary>The constant 'BernoulliSample0'</summary>
		public bool[] BernoulliSample0;
		#endregion

		#region Properties
		/// <summary>The number of iterations done from the initial state</summary>
		public int NumberOfIterationsDone
		{			get {
				return this.numberOfIterationsDone;
			}
		}

		/// <summary>The externally-specified value of 'cst'</summary>
		public int cst
		{			get {
				return this.Cst;
			}
			set {
				if (this.Cst!=value) {
					this.Cst = value;
					this.numberOfIterationsDone = 0;
					this.Changed_cst_obs1_cst1_cst0_iterationsDone = 0;
				}
			}
		}

		/// <summary>The externally-specified value of 'obs1'</summary>
		public bool[] obs1
		{			get {
				return this.Obs1;
			}
			set {
				if ((value!=null)&&(value.Length!=this.Cst)) {
					throw new ArgumentException(((("Provided array of length "+value.Length)+" when length ")+this.Cst)+" was expected for variable \'obs1\'");
				}
				this.Obs1 = value;
				this.numberOfIterationsDone = 0;
				this.Changed_cst_obs1_cst1_cst0_iterationsDone = 0;
			}
		}

		/// <summary>The externally-specified value of 'cst1'</summary>
		public double cst1
		{			get {
				return this.Cst1;
			}
			set {
				if (this.Cst1!=value) {
					this.Cst1 = value;
					this.numberOfIterationsDone = 0;
					this.Changed_cst_obs1_cst1_cst0_iterationsDone = 0;
				}
			}
		}

		/// <summary>The externally-specified value of 'cst0'</summary>
		public double cst0
		{			get {
				return this.Cst0;
			}
			set {
				if (this.Cst0!=value) {
					this.Cst0 = value;
					this.numberOfIterationsDone = 0;
					this.Changed_cst_obs1_cst1_cst0_iterationsDone = 0;
				}
			}
		}

		#endregion

		#region Methods
		/// <summary>Get the observed value of the specified variable.</summary>
		/// <param name="variableName">Variable name</param>
		public object GetObservedValue(string variableName)
		{
			if (variableName=="cst") {
				return this.cst;
			}
			if (variableName=="obs1") {
				return this.obs1;
			}
			if (variableName=="cst1") {
				return this.cst1;
			}
			if (variableName=="cst0") {
				return this.cst0;
			}
			throw new ArgumentException("Not an observed variable name: "+variableName);
		}

		/// <summary>Set the observed value of the specified variable.</summary>
		/// <param name="variableName">Variable name</param>
		/// <param name="value">Observed value</param>
		public void SetObservedValue(string variableName, object value)
		{
			if (variableName=="cst") {
				this.cst = (int)value;
				return ;
			}
			if (variableName=="obs1") {
				this.obs1 = (bool[])value;
				return ;
			}
			if (variableName=="cst1") {
				this.cst1 = (double)value;
				return ;
			}
			if (variableName=="cst0") {
				this.cst0 = (double)value;
				return ;
			}
			throw new ArgumentException("Not an observed variable name: "+variableName);
		}

		/// <summary>The marginal distribution of the specified variable.</summary>
		/// <param name="variableName">Variable name</param>
		public object Marginal(string variableName)
		{
			if (variableName=="cst") {
				return this.CstMarginal();
			}
			if (variableName=="obs1") {
				return this.Obs1Marginal();
			}
			if (variableName=="cst1") {
				return this.Cst1Marginal();
			}
			if (variableName=="cst0") {
				return this.Cst0Marginal();
			}
			if (variableName=="BetaSample0") {
				return this.BetaSample0Marginal();
			}
			if (variableName=="BernoulliSample0") {
				return this.BernoulliSample0Marginal();
			}
			throw new ArgumentException("This class was not built to infer "+variableName);
		}

		public T Marginal<T>(string variableName)
		{
			return Distribution.ChangeType<T>(this.Marginal(variableName));
		}

		/// <summary>The query-specific marginal distribution of the specified variable.</summary>
		/// <param name="variableName">Variable name</param>
		/// <param name="query">QueryType name. For example, GibbsSampling answers 'Marginal', 'Samples', and 'Conditionals' queries</param>
		public object Marginal(string variableName, string query)
		{
			if (query=="Marginal") {
				return this.Marginal(variableName);
			}
			throw new ArgumentException(((("This class was not built to infer \'"+variableName)+"\' with query \'")+query)+"\'");
		}

		public T Marginal<T>(string variableName, string query)
		{
			return Distribution.ChangeType<T>(this.Marginal(variableName, query));
		}

		/// <summary>Update all marginals, by iterating message passing the given number of times</summary>
		/// <param name="numberOfIterations">The number of times to iterate each loop</param>
		/// <param name="initialise">If true, messages that initialise loops are reset when observed values change</param>
		private void Execute(int numberOfIterations, bool initialise)
		{
			this.Changed_cst_obs1_cst1_cst0();
			this.numberOfIterationsDone = numberOfIterations;
		}

		public void Execute(int numberOfIterations)
		{
			this.Execute(numberOfIterations, true);
		}

		public void Update(int additionalIterations)
		{
			this.Execute(this.numberOfIterationsDone+additionalIterations, false);
		}

		private void OnProgressChanged(ProgressChangedEventArgs e)
		{
			// Make a temporary copy of the event to avoid a race condition
			// if the last subscriber unsubscribes immediately after the null check and before the event is raised.
			EventHandler<ProgressChangedEventArgs> handler = this.ProgressChanged;
			if (handler!=null) {
				handler(this, e);
			}
		}

		/// <summary>Reset all messages to their initial values.  Sets NumberOfIterationsDone to 0.</summary>
		public void Reset()
		{
			this.Execute(0);
		}

		/// <summary>Computations that depend on the observed value of cst and obs1 and cst1 and cst0</summary>
		public void Changed_cst_obs1_cst1_cst0()
		{
			if (this.Changed_cst_obs1_cst1_cst0_iterationsDone==1) {
				return ;
			}
			this.cst_marginal = new PointMass<int>(this.Cst);
			this.obs1_marginal = new DistributionStructArray<Bernoulli,bool>(this.Cst, delegate(int _range1) {
				return Bernoulli.Uniform();
			});
			this.obs1_marginal = Distribution.SetPoint<DistributionStructArray<Bernoulli,bool>,bool[]>(this.obs1_marginal, this.Obs1);
			this.cst1_marginal = Gaussian.Uniform();
			this.cst1_marginal = Distribution.SetPoint<Gaussian,double>(this.cst1_marginal, this.Cst1);
			this.cst0_marginal = Gaussian.Uniform();
			this.cst0_marginal = Distribution.SetPoint<Gaussian,double>(this.cst0_marginal, this.Cst0);
			Beta BetaSample0_F = Beta.Uniform();
			this.BetaSample0_marginal_F = Beta.Uniform();
			// Message to 'BetaSample0' from Sample factor
			BetaSample0_F = BetaFromTrueAndFalseCountsOp.SampleAverageConditional(this.Cst0, this.Cst1);
			DistributionStructArray<Beta,double> BetaSample0_rep_B = default(DistributionStructArray<Beta,double>);
			// Create array for 'BetaSample0_rep' Backwards messages.
			BetaSample0_rep_B = new DistributionStructArray<Beta,double>(this.Cst);
			for(int _range1 = 0; _range1<this.Cst; _range1++) {
				BetaSample0_rep_B[_range1] = Beta.Uniform();
				// Message to 'BetaSample0_rep' from Bernoulli factor
				BetaSample0_rep_B[_range1] = BernoulliFromBetaOp.ProbTrueAverageConditional(this.Obs1[_range1]);
			}
			// Buffer for ReplicateOp_Divide.Marginal<Beta>
			Beta BetaSample0_rep_B_toDef = default(Beta);
			// Message to 'BetaSample0_rep' from Replicate factor
			BetaSample0_rep_B_toDef = ReplicateOp_Divide.ToDefInit<Beta>(BetaSample0_F);
			// Message to 'BetaSample0_rep' from Replicate factor
			BetaSample0_rep_B_toDef = ReplicateOp_Divide.ToDef<Beta>(BetaSample0_rep_B, BetaSample0_rep_B_toDef);
			// Message to 'BetaSample0_marginal' from Variable factor
			this.BetaSample0_marginal_F = VariableOp.MarginalAverageConditional<Beta>(BetaSample0_rep_B_toDef, BetaSample0_F, this.BetaSample0_marginal_F);
			this.BernoulliSample0_marginal = new DistributionStructArray<Bernoulli,bool>(this.Cst, delegate(int _range1) {
				return Bernoulli.Uniform();
			});
			this.BernoulliSample0 = new bool[this.Cst];
			for(int _range1 = 0; _range1<this.Cst; _range1++) {
				this.BernoulliSample0[_range1] = this.Obs1[_range1];
			}
			this.BernoulliSample0_marginal = Distribution.SetPoint<DistributionStructArray<Bernoulli,bool>,bool[]>(this.BernoulliSample0_marginal, this.BernoulliSample0);
			this.Changed_cst_obs1_cst1_cst0_iterationsDone = 1;
		}

		/// <summary>
		/// Returns the marginal distribution for 'cst' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public PointMass<int> CstMarginal()
		{
			return this.cst_marginal;
		}

		/// <summary>
		/// Returns the marginal distribution for 'obs1' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public DistributionStructArray<Bernoulli,bool> Obs1Marginal()
		{
			return this.obs1_marginal;
		}

		/// <summary>
		/// Returns the marginal distribution for 'cst1' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public Gaussian Cst1Marginal()
		{
			return this.cst1_marginal;
		}

		/// <summary>
		/// Returns the marginal distribution for 'cst0' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public Gaussian Cst0Marginal()
		{
			return this.cst0_marginal;
		}

		/// <summary>
		/// Returns the marginal distribution for 'BetaSample0' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public Beta BetaSample0Marginal()
		{
			return this.BetaSample0_marginal_F;
		}

		/// <summary>
		/// Returns the marginal distribution for 'BernoulliSample0' given by the current state of the
		/// message passing algorithm.
		/// </summary>
		/// <returns>The marginal distribution</returns>
		public DistributionStructArray<Bernoulli,bool> BernoulliSample0Marginal()
		{
			return this.BernoulliSample0_marginal;
		}

		#endregion

		#region Events
		/// <summary>Event that is fired when the progress of inference changes, typically at the end of one iteration of the inference algorithm.</summary>
		public event EventHandler<ProgressChangedEventArgs> ProgressChanged;
		#endregion

	}

}
